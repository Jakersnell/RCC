mod binary_expressions;
mod casting;
mod declarations;
mod expressions;
pub mod hlir;
mod statements;
mod symbols;

use crate::analysis::hlir::*;
use crate::analysis::symbols::SymbolResolver;
use crate::parser::ast::*;
use crate::util::error::{CompilerError, CompilerWarning, Reporter};
use crate::util::{Locatable, Span};
use std::cell::RefCell;
use std::rc::Rc;

pub type SharedReporter = Rc<RefCell<Reporter>>;

pub struct GlobalValidator {
    ast: AbstractSyntaxTree,
    scope: SymbolResolver,
    reporter: SharedReporter,
}

impl GlobalValidator {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        Self {
            ast,
            scope: SymbolResolver::create_root(),
            reporter: Rc::new(RefCell::new(Reporter::default())),
        }
    }

    pub fn validate(mut self) -> Result<HighLevelIR, SharedReporter> {
        let hlir = HighLevelIR::default();
        for node in &*self.ast {
            use crate::parser::ast::InitDeclaration::*;
            match node {
                Declaration(locatable_variable) => {
                    todo!("validation")
                }
                Function(locatable_function) => {
                    todo!("validation")
                }
                Struct(locatable_struct) => {
                    todo!("validation")
                }
            }
        }
        if self.reporter.borrow().status().is_err() {
            Err(self.reporter)
        } else {
            Ok(hlir)
        }
    }

    fn report_error(&mut self, error: CompilerError) -> Result<(), ()> {
        self.reporter.borrow_mut().report_error(error);
        Err(())
    }

    fn report_warning(&mut self, warning: CompilerWarning) {
        self.reporter.borrow_mut().report_warning(warning);
    }

    fn push_scope(&mut self) {
        let mut resolver = SymbolResolver::default(); // blank temp resolver
        std::mem::swap(&mut resolver, &mut self.scope);
        resolver = SymbolResolver::new(Some(Box::new(resolver)));
        std::mem::swap(&mut resolver, &mut self.scope);
        debug_assert!(resolver.parent.is_none());
        debug_assert!(resolver.symbols.is_empty());
    }

    fn pop_scope(&mut self) {
        let mut resolver = SymbolResolver::default(); // blank temp resolver
        std::mem::swap(&mut resolver, &mut self.scope);
        resolver = *resolver
            .remove_self()
            .expect("Popped scope on global scope.");
        std::mem::swap(&mut resolver, &mut self.scope);
        debug_assert!(resolver.parent.is_none());
        debug_assert!(resolver.symbols.is_empty());
    }

    fn validate_statement(&mut self, stmt: &Locatable<Statement>) -> Result<HlirStmt, ()> {
        match &stmt.value {
            Statement::Expression(expr) => {
                Ok(HlirStmt::Expression(self.validate_expression(&expr.value)?))
            }
            Statement::Declaration(var_dec) => {
                todo!()
            }
            Statement::If(condition, body, else_body) => {
                todo!()
            }
            Statement::While(condition, body) => {
                todo!()
            }
            Statement::For(initializer, condition, post_loop, body) => {
                todo!("break this into a while loop")
            }
            Statement::Break => {
                todo!()
            }
            Statement::Continue => {
                todo!()
            }
            Statement::Return(value) => {
                todo!()
            }
            Statement::Block(block) => {
                todo!()
            }
            Statement::Empty => {
                todo!()
            }
        }
    }

    /*
    State machine to validate a type from a DeclarationSpecifier
     */
    fn validate_type(
        &mut self,
        declaration: &DeclarationSpecifier,
        location: Span,
    ) -> Result<HlirType, ()> {
        #[derive(Debug, PartialEq)]
        enum State {
            Start,
            SeenUnsigned,
            SeenSigned,
            End,
        }
        let mut hlir_type: Option<HlirTypeKind> = None;
        let mut state = State::Start;
        let mut iter = declaration.ty.iter();
        macro_rules! seen_signed_or_unsigned {
            ($ty_spec:ident, $unsigned:literal) => {
                match $ty_spec {
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(HlirTypeKind::Char($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Int($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(HlirTypeKind::Long($unsigned));
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
                        hlir_type = Some(HlirTypeKind::Void);
                        state = State::End;
                    }
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(HlirTypeKind::Char(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Int(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(HlirTypeKind::Long(false));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Double) => {
                        hlir_type = Some(HlirTypeKind::Double);
                        state = State::End;
                    }
                    Some(TypeSpecifier::Struct(ident)) => {
                        hlir_type = Some(HlirTypeKind::Struct(ident.clone()));
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

        let ty_dec = if declaration.pointer {
            HlirTypeDecl::Pointer
        } else {
            HlirTypeDecl::Basic
        };

        if matches!(ty_kind, HlirTypeKind::Void) && !matches!(ty_dec, HlirTypeDecl::Pointer) {
            self.report_error(CompilerError::IncompleteType(location));
        }

        Ok(HlirType {
            kind: ty_kind,
            decl: ty_dec,
        })
    }
}

#[cfg(test)]
macro_rules! make_dec_specifier {
    ($types:expr) => {
        DeclarationSpecifier {
            specifiers: vec![],
            qualifiers: vec![],
            ty: $types,
            pointer: false,
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
    ];
    for types in type_tests {
        let mut validator = GlobalValidator::new(AbstractSyntaxTree::default());
        let types = make_dec_specifier!(types);
        let result = validator.validate_type(&types, Span::default());
        if result.is_ok() {
            panic!("Expected error, got ok, test: {:?}", types);
        }
    }
}

#[test]
fn test_validate_type_returns_ok_for_valid_type_orientations() {
    use TypeSpecifier::*;
    let type_tests = [
        (vec![Int], HlirTypeKind::Int(false)),
        (vec![Long], HlirTypeKind::Long(false)),
        (vec![Unsigned, Int], HlirTypeKind::Int(true)),
        (vec![Unsigned, Long], HlirTypeKind::Long(true)),
        (vec![Signed, Int], HlirTypeKind::Int(false)),
        (vec![Signed, Long], HlirTypeKind::Long(false)),
        (vec![Double], HlirTypeKind::Double),
        (vec![Void], HlirTypeKind::Void),
    ];
    for (types, expected) in type_tests {
        let mut validator = GlobalValidator::new(AbstractSyntaxTree::default());
        let dec_spec = make_dec_specifier!(types);
        let expected = HlirType {
            decl: HlirTypeDecl::Basic,
            kind: expected,
        };
        let result = validator.validate_type(&dec_spec, Span::default());
        assert_eq!(result, Ok(expected));
    }
}
