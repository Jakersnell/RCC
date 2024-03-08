mod flow;
pub mod hlir;
mod symbols;

use crate::analysis::hlir::{HighLevelIR, HlirType, HlirTypeKind};
use crate::analysis::symbols::SymbolResolver;
use crate::parser::ast::{
    ASTRoot, AbstractSyntaxTree, Block, DeclarationSpecifier, InitDeclaration, TypeQualifier,
    TypeSpecifier, VariableDeclaration,
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
    reporter: SharedReporter,
}

impl<'a> GlobalValidator<'a> {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        Self {
            ast,
            scope: SymbolResolver::create_root(),
            reporter: Rc::new(RefCell::new(Reporter::default())),
        }
    }

    fn report_error(&mut self, error: CompilerError) {
        self.reporter.borrow_mut().report_error(error);
    }

    fn report_warning(&mut self, warning: CompilerWarning) {
        self.reporter.borrow_mut().report_warning(warning);
    }

    pub fn validate(mut self) -> Result<HighLevelIR, Reporter> {
        let hlir = HighLevelIR::default();
        for node in &*self.ast {
            use crate::parser::ast::InitDeclaration::*;
            match node {
                Declaration(locatable_variable) => {}
                Function(locatable_function) => {}
                Struct(locatable_struct) => {}
            }
        }
        todo!("validation")
    }

    fn validate_variable(
        &mut self,
        locatable_variable: &Locatable<VariableDeclaration>,
    ) -> Result<(), ()> {
        let span = locatable_variable.location;
        let var = &locatable_variable.value;

        // validate declaration for the variable
        // the declaration must have an ident.
        // validate the declaration specifier
        // for now all storage specifiers are ignored, report a warning displaying this
        // the only type qualifier is const, if we see it, it is counted, anything after is redundant
        // type specifier feels like it may be similar to a finite state machine
        // if we see signed or unsigned we expect one of the following after, char, int, long,
        // anything else is an error
        // anything coming after a type otherwise is an error, except for Long, because of Long long

        // if is array it will probably need a size
        // if is array with no size the size must be calculated via the assignment,
        // and thus requires an assignment

        let declaration = &var.declaration;
        if declaration.value.ident.is_none() {
            let err = CompilerError::DeclarationMissingIdentifier(declaration.location);
            self.report_error(err);
            return Err(());
        }

        let ident = declaration.value.ident.as_ref().unwrap();
        let ident_span = ident.location;
        let ident = ident.value.clone();

        let specifier = &declaration.specifier;
        let specifier_span = specifier.location;
        let specifier = &specifier.value;

        for storage_spec in &specifier.specifiers {
            // need to change this span to the specific storage spec location
            self.report_warning(CompilerWarning::UnsupportedStorageSpecifier(
                storage_spec.to_string(),
                specifier_span,
            ))
        }

        let mut is_const = false;
        for ty_qual in &specifier.qualifiers {
            // this is set up for expansion
            match ty_qual {
                TypeQualifier::Const => {
                    if is_const {
                        let warning =
                            CompilerWarning::RedundantUsage(ty_qual.to_string(), specifier_span);
                        self.report_warning(warning);
                    } else {
                        is_const = true;
                    }
                }
            }
        }
        let is_const = is_const; // remove mutability

        Ok(())
    }

    fn validate_type_specifiers(
        &mut self,
        specifiers: &[TypeSpecifier],
        location: Span,
    ) -> Result<HlirType, ()> {
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
                State::SeenUnsigned => match ty_spec {
                    Some(TypeSpecifier::Char) => {
                        hlir_type = Some(HlirTypeKind::Char(true));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Int(true));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        state = State::SeenUnsignedLong;
                    }
                    Some(ty) => {
                        let err = CompilerError::TypeCannotBeSignedOrUnsigned(
                            ty.to_string(),
                            location,
                        );
                        self.report_error(err);
                        return Err(());
                    }
                    None => {
                        let err = CompilerError::ExpectedTypeSpecifier(location);
                        self.report_error(err);
                        return Err(());
                    }

                },
                State::SeenUnsignedLong => match ty_spec {
                    Some(TypeSpecifier::Int) => {
                        hlir_type = Some(HlirTypeKind::Long(true));
                        state = State::End;
                    }
                    Some(TypeSpecifier::Long) => {
                        hlir_type = Some(HlirTypeKind::LLong(true));
                        state = State::End;
                    }
                    Some(ty) => {
                            let err = CompilerError::TypeCannotBeSignedOrUnsigned(
                                ty.to_string(),
                                location,
                            );
                            self.report_error(err);
                        return Err(());
                    }
                    None => {
                        hlir_type = Some(HlirTypeKind::Long(true));
                        state = State::End;
                    }
                },
                State::SeenSigned => match ty_spec {
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
                    Some(ty) => {
                        let err = CompilerError::TypeCannotBeSignedOrUnsigned(
                            ty.to_string(),
                            location,
                        );
                        self.report_error(err);
                        return Err(());
                    }
                    None => {
                        let err = CompilerError::ExpectedTypeSpecifier(location);
                        self.report_error(err);
                        return Err(());
                    }
                },
                State::SeenSignedLong => {}
                State::End => {
                    debug_assert!(hlir_type.is_some());
                    break;
                }
            }
        }
        todo!()
    }
}
