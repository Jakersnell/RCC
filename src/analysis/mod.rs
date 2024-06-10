use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::iter::Map;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::analysis::symbols::SymbolResolver;
use crate::data::ast::*;
use crate::data::mlir::*;
use crate::util::{Locatable, Span};
use crate::util::error::{CompilerError, CompilerWarning, Reporter};
use crate::util::str_intern::InternedStr;

mod binary_expressions;
mod casting;
mod constant_folding;
mod control_flow;
mod declarations;
mod expressions;
mod statements;
mod symbols;

static mut LABEL_COUNT: usize = 0;

pub(in crate::analysis) fn peek_current_label() -> usize {
    unsafe { LABEL_COUNT }
}

pub(in crate::analysis) fn peek_next_label() -> usize {
    peek_current_label() + 1
}

pub(in crate::analysis) fn create_label() -> usize {
    unsafe {
        LABEL_COUNT += 1;
        LABEL_COUNT
    }
}

#[derive(Debug, Default)]
pub struct SharedReporter(Rc<RefCell<Reporter>>);

impl Deref for SharedReporter {
    type Target = Rc<RefCell<Reporter>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Analyzer {
    ast: Option<AbstractSyntaxTree>,
    scope: Box<RefCell<SymbolResolver>>,
    reporter: SharedReporter,
    return_ty: Option<MlirType>, // for functions
    loop_label_stack: VecDeque<InternedStr>,
}

impl Analyzer {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        Self {
            ast: Some(ast),
            scope: Box::new(RefCell::new(SymbolResolver::create_root())),
            reporter: SharedReporter::default(),
            return_ty: None,
            loop_label_stack: VecDeque::new(),
        }
    }

    pub fn validate(mut self) -> Result<MlirModule, SharedReporter> {
        let mut globals = Vec::new();
        let mut functions = BTreeMap::new();
        let mut structs = Vec::new();
        let ast = self.ast.take().expect("Ast must be Some(T)");
        macro_rules! push_locatable {
            ($map:expr, $func:expr, $locatable:ident) => {
                #[allow(clippy::redundant_closure_call)]
                if let Ok(result) = $func($locatable) {
                    $map.insert(
                        result.ident.clone(),
                        $locatable.location.into_locatable(result),
                    );
                }
            };
        }
        for node in &*ast {
            use crate::data::ast::InitDeclaration::*;
            match node {
                Declaration(locatable_variable) => {
                    if let Ok(mut var) = self.validate_variable_declaration(locatable_variable) {
                        self.add_variable_to_scope(&mut var, locatable_variable.location);
                        globals.push(var);
                    }
                }
                Function(locatable_function) => {
                    push_locatable!(
                        functions,
                        |item| self.validate_function_definition(item),
                        locatable_function
                    )
                }
                Struct(locatable_struct) => {
                    if let Ok(_struct) = self.validate_struct_definition(locatable_struct) {
                        structs.push(_struct);
                    }
                }
            }
        }

        let idents = self.scope.borrow().get_unused_idents();

        for (ident, span) in idents {
            self.report_warning(CompilerWarning::UnusedItem(ident.to_string(), span))
        }

        fn deref_map<K: Eq + std::hash::Hash + std::cmp::Ord, T>(
            map: BTreeMap<K, Locatable<T>>,
        ) -> BTreeMap<K, T> {
            let mut dest = BTreeMap::new();
            for (key, value) in map.into_iter() {
                dest.insert(key, value.value);
            }
            dest
        }

        let functions = deref_map(functions);

        if self.reporter.borrow().status().is_ok() && !functions.contains_key("main") {
            self.report_error(CompilerError::MissingMain);
        }

        if self.reporter.borrow().status().is_err() {
            Err(self.reporter)
        } else {
            Ok(MlirModule {
                functions,
                structs,
                globals,
            })
        }
    }

    fn report_error(&mut self, error: CompilerError) -> Result<(), ()> {
        self.reporter.0.borrow_mut().report_error(error);
        Err(())
    }

    fn report_warning(&mut self, warning: CompilerWarning) {
        self.reporter.0.borrow_mut().report_warning(warning);
    }

    fn add_variable_to_scope(&mut self, var: &mut MlirVariable, span: Span) -> Result<(), ()> {
        let array_size = match &var.ty.decl {
            MlirTypeDecl::Array(size) => Some(*size),
            _ => None,
        };
        let result = self.scope.borrow_mut().add_variable(var, span);
        if let Err(err) = result {
            self.report_error(err);
        }
        Ok(())
    }

    fn push_scope(&mut self) {
        let mut resolver = SymbolResolver::default(); // blank temp resolver
        resolver = self.scope.replace(resolver);
        resolver = SymbolResolver::new(Some(Box::new(RefCell::new(resolver))));
        resolver = self.scope.replace(resolver);
        debug_assert!(resolver.parent.is_none());
        debug_assert!(resolver.symbols.is_empty());
    }

    fn pop_scope(&mut self) {
        self.report_unused_items();
        let mut resolver = SymbolResolver::default(); // blank temp resolver
        resolver = self.scope.replace(resolver);
        resolver = resolver
            .remove_self()
            .expect("Popped scope on global scope.")
            .take();
        resolver = self.scope.replace(resolver);
        debug_assert!(resolver.parent.is_none());
        debug_assert!(resolver.symbols.is_empty());
    }

    fn report_unused_items(&mut self) {
        let items = self.scope.borrow_mut().get_unused_idents();
        for (item, span) in items {
            if cfg!(debug_assertions) {
                assert!(self
                    .scope
                    .borrow_mut()
                    .get_variable_type_and_id(&item, span)
                    .is_ok());
            }
            self.report_warning(CompilerWarning::UnusedVariable(span));
        }
    }

    fn validate_type(
        &mut self,
        declaration: &DeclarationSpecifier,
        location: Span,
        is_function_return_ty: bool,
        is_struct_dec: bool,
    ) -> Result<MlirType, ()> {
        #[derive(Debug, PartialEq)]
        enum State {
            Start,
            SeenUnsigned,
            SeenSigned,
            End,
        }
        let mut hlir_type: Option<MlirTypeKind> = None;
        let mut state = State::Start;
        let mut iter = declaration.ty.iter();
        macro_rules! seen_signed_or_unsigned {
            ($ty_spec:ident, $unsigned:literal) => {
                match $ty_spec {
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(MlirTypeKind::Char($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(MlirTypeKind::Int($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(MlirTypeKind::Long($unsigned));
                        state = State::End;
                    }
                    Some(ty) => {
                        let err =
                            CompilerError::TypeCannotBeSignedOrUnsigned(ty.to_string(), location);
                        self.report_error(err);
                        return Err(());
                    }
                    None => {
                        let err = CompilerError::ExpectedTypeSpecifier(location);
                        self.report_error(err);
                        return Err(());
                    }
                }
            };
        }

        loop {
            let mut ty_spec = if state != State::End {
                iter.next()
            } else {
                None
            };

            match state {
                State::Start => match ty_spec {
                    Some(TypeSpecifier::Void) => {
                        hlir_type = Some(MlirTypeKind::Void);
                        state = State::End;
                    }
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(MlirTypeKind::Char(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(MlirTypeKind::Int(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(MlirTypeKind::Long(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Double) => {
                        hlir_type = Some(MlirTypeKind::Double);
                        state = State::End;
                    }
                    Some(TypeSpecifier::Struct(ident)) => {
                        hlir_type = Some(MlirTypeKind::Struct(ident.clone()));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Signed) => {
                        state = State::SeenSigned;
                    }
                    Some(TypeSpecifier::Unsigned) => {
                        state = State::SeenUnsigned;
                    }
                    None => panic!("Span: {:?}", location),
                },
                State::SeenUnsigned => seen_signed_or_unsigned!(ty_spec, true),
                State::SeenSigned => seen_signed_or_unsigned!(ty_spec, false),
                State::End => break,
            }
        }
        debug_assert!(hlir_type.is_some());
        if iter.next().is_some() {
            let err = CompilerError::InvalidTypeSpecifier(location);
            self.report_error(err);
            return Err(());
        }
        let ty_kind = hlir_type.unwrap();

        match &ty_kind {
            MlirTypeKind::Struct(ident) if !is_struct_dec => {
                let result = self.scope.borrow_mut().check_struct_exists(ident, location);
                if let Err(err) = result {
                    self.report_error(err);
                    return Err(());
                }
            }
            _ => (),
        }

        let ty_dec = if declaration.pointer {
            MlirTypeDecl::Pointer
        } else {
            MlirTypeDecl::Basic
        };

        if !is_function_return_ty
            && matches!(ty_kind, MlirTypeKind::Void)
            && !matches!(ty_dec, MlirTypeDecl::Pointer)
        {
            self.report_error(CompilerError::IncompleteType(location));
            return Err(());
        }

        Ok(MlirType {
            kind: ty_kind,
            decl: ty_dec,
        })
    }
}

#[cfg(test)]
macro_rules! make_dec_specifier {
    ($types:expr, $is_ptr:expr) => {
        DeclarationSpecifier {
            specifiers: vec![],
            qualifiers: vec![],
            ty: $types,
            pointer: $is_ptr,
        }
    };
}

#[test]
fn test_validate_type_returns_error_for_invalid_type_orientations() {
    use TypeSpecifier::*;
    let type_tests = [
        vec![Int, Int, Int],
        vec![Long, Int, Int],
        vec![Int, Long, Int],
        vec![Unsigned, Double],
        vec![Unsigned, Signed, Int],
        vec![Signed, Unsigned, Long],
        vec![Signed, Double],
        vec![Void],
    ];
    for types in type_tests {
        let mut validator = Analyzer::new(AbstractSyntaxTree::default());
        let types = make_dec_specifier!(types, false);
        let result = validator.validate_type(&types, Span::default(), false, false);
        if result.is_ok() {
            panic!("Expected error, got ok, test: {:?}", types);
        }
    }
}

#[test]
fn test_validate_type_returns_ok_for_valid_type_orientations() {
    use TypeSpecifier::*;
    let type_tests = [
        (vec![Int], MlirTypeKind::Int(false), MlirTypeDecl::Basic),
        (vec![Long], MlirTypeKind::Long(false), MlirTypeDecl::Basic),
        (
            vec![Unsigned, Int],
            MlirTypeKind::Int(true),
            MlirTypeDecl::Basic,
        ),
        (
            vec![Unsigned, Long],
            MlirTypeKind::Long(true),
            MlirTypeDecl::Basic,
        ),
        (
            vec![Signed, Int],
            MlirTypeKind::Int(false),
            MlirTypeDecl::Basic,
        ),
        (
            vec![Signed, Long],
            MlirTypeKind::Long(false),
            MlirTypeDecl::Basic,
        ),
        (vec![Double], MlirTypeKind::Double, MlirTypeDecl::Basic),
        (vec![Void], MlirTypeKind::Void, MlirTypeDecl::Pointer),
    ];
    for (types, expected, decl) in type_tests {
        let mut validator = Analyzer::new(AbstractSyntaxTree::default());
        let dec_spec = make_dec_specifier!(types, decl == MlirTypeDecl::Pointer);
        let expected = MlirType {
            decl,
            kind: expected,
        };
        let result = validator.validate_type(&dec_spec, Span::default(), false, false);
        assert_eq!(result, Ok(expected));
    }
}

#[cfg(test)]
mod tests {
    use crate::{analysis, lexer, parser};
    use crate::analysis::SharedReporter;
    use crate::data::mlir::MlirModule;

    pub(in crate::analysis) fn run_analysis_test(path: &str) -> Result<MlirModule, SharedReporter> {
        let source = std::fs::read_to_string(path).expect("Could not read file.");
        let lexer = lexer::Lexer::new(source.into())
            .lex_all()
            .unwrap()
            .into_iter();
        let parser = parser::Parser::new(lexer);
        let result = parser.parse_all().expect("Error in Parser.");
        analysis::Analyzer::new(result).validate()
    }
}
