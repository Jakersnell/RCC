mod flow;
pub mod hlir;
mod symbols;

use crate::analysis::hlir::{HighLevelIR, HlirExpr, HlirType, HlirTypeKind, HlirVariable};
use crate::analysis::symbols::SymbolResolver;
use crate::parser::ast::{
    ASTRoot, AbstractSyntaxTree, Block, DeclarationSpecifier, Expression, InitDeclaration,
    TypeQualifier, TypeSpecifier, VariableDeclaration,
};
use crate::util::error::{CompilerError, CompilerWarning, Reporter};
use crate::util::str_intern::InternedStr;
use crate::util::{Locatable, Span};
use derive_new::new;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type SharedReporter = Rc<RefCell<Reporter>>;

pub struct GlobalValidator<'a> {
    ast: AbstractSyntaxTree,
    scope: SymbolResolver<'a>,
    function_scope: Option<SymbolResolver<'a>>,
    reporter: SharedReporter,
}

impl<'a> GlobalValidator<'a> {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        Self {
            ast,
            scope: SymbolResolver::create_root(),
            function_scope: None,
            reporter: Rc::new(RefCell::new(Reporter::default())),
        }
    }

    fn report_error(&mut self, error: CompilerError) {
        self.reporter.borrow_mut().report_error(error);
    }

    fn report_warning(&mut self, warning: CompilerWarning) {
        self.reporter.borrow_mut().report_warning(warning);
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

    fn validate_variable(
        &mut self,
        locatable_variable: &Locatable<VariableDeclaration>,
    ) -> Result<HlirVariable, ()> {
        let span = locatable_variable.location;
        let var = &locatable_variable.value;

        let declaration = &var.declaration;
        if declaration.value.ident.is_none() {
            let err = CompilerError::DeclarationMissingIdentifier(declaration.location);
            self.report_error(err);
            return Err(());
        }

        let ident = declaration
            .value
            .ident
            .as_ref()
            .expect("Fatal compiler error: Identifier not set");
        let ident_span = ident.location;
        let ident = ident.value.clone();

        let ty = self.validate_variable_specifier(&declaration.value.specifier, span)?;
        if var.is_array && var.array_size.is_none() && var.initializer.is_none() {
            let err = CompilerError::ArraySizeNotSpecified(span);
            self.report_error(err);
            return Err(());
        }

        let initializer = if let Some(init) = &var.initializer {
            todo!("Validate variable initializer here!");
        } else {
            None
        };

        Ok(HlirVariable {
            ty,
            ident,
            is_array: var.is_array,
            initializer,
        })
    }

    fn validate_variable_specifier(
        &mut self,
        specifier: &DeclarationSpecifier,
        span: Span,
    ) -> Result<HlirType, ()> {
        for storage_spec in &specifier.specifiers {
            // need to change this span to the specific storage spec location
            self.report_warning(CompilerWarning::UnsupportedStorageSpecifier(
                storage_spec.to_string(),
                span,
            ))
        }

        let mut is_const = false;
        for ty_qual in &specifier.qualifiers {
            // this is set up for expansion
            match ty_qual {
                TypeQualifier::Const => {
                    if is_const {
                        let warning = CompilerWarning::RedundantUsage(ty_qual.to_string(), span);
                        self.report_warning(warning);
                    } else {
                        is_const = true;
                    }
                }
            }
        }

        let ty_kind = self.validate_type(&specifier.ty, span)?;
        Ok(HlirType {
            kind: ty_kind,
            pointer: specifier.pointer,
        })
    }

    fn validate_type(
        &mut self,
        specifiers: &[TypeSpecifier],
        location: Span,
    ) -> Result<HlirTypeKind, ()> {
        enum State {
            Start,
            SeenUnsigned,
            SeenUnsignedLong,
            SeenSigned,
            SeenSignedLong,
            End,
        }
        let mut hlir_type: Option<HlirTypeKind> = None;
        let mut state = State::Start;
        let mut iter = specifiers.iter();
        macro_rules! seen_signed_or_unsigned {
            ($ty_spec:ident, $unsigned:literal, $long:expr) => {
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
                        state = $long;
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
        macro_rules! seen_signed_or_unsigned_long {
            ($ty_spec:ident, $unsigned:literal) => {
                match $ty_spec {
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Long($unsigned));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(HlirTypeKind::LLong($unsigned));
                        state = State::End;
                    }
                    Some(ty) => {
                        let err =
                            CompilerError::TypeCannotBeSignedOrUnsigned(ty.to_string(), location);
                        self.report_error(err);
                        return Err(());
                    }
                    None => {
                        hlir_type = Some(HlirTypeKind::Long($unsigned));
                        state = State::End;
                    }
                }
            };
        }
        loop {
            let ty_spec = iter.next();
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
                        state = State::SeenSignedLong;
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
                    None => panic!(
                        "Fatal compiler error: Missing type specifier at state start. src-location: {:?}",
                        location
                    ),
                },
                State::SeenUnsigned => seen_signed_or_unsigned!(ty_spec, true, State::SeenUnsignedLong),
                State::SeenSigned => seen_signed_or_unsigned!(ty_spec, false, State::SeenSignedLong),
                State::SeenUnsignedLong => seen_signed_or_unsigned_long!(ty_spec, true),
                State::SeenSignedLong => seen_signed_or_unsigned_long!(ty_spec, false),
                State::End => {
                    break;
                }
            }
        }
        debug_assert!(hlir_type.is_some());
        let ty = hlir_type.expect("Fatal compiler error: Type specifier not set");
        Ok(ty)
    }

    // Expression Validation

    fn validate_primary_expression(
        &mut self,
        expr: &Expression,
        span: Span,
    ) -> Result<HlirExpr, ()> {
        todo!();
    }
}
