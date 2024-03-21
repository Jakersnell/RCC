use crate::analysis::hlir::{
    HighLevelIR, HlirBlock, HlirFunction, HlirStmt, HlirType, HlirTypeDecl, HlirTypeKind,
    HlirVarInit, HlirVariable,
};
use crate::analysis::symbols::SymbolResolver;
use crate::analysis::{GlobalValidator, SharedReporter};
use crate::parser::ast::{
    AbstractSyntaxTree, Block, Declaration, DeclarationSpecifier, Expression, FunctionDeclaration,
    Statement, TypeQualifier, TypeSpecifier, VariableDeclaration,
};
use crate::util::error::{CompilerError, CompilerWarning, Reporter};
use crate::util::{Locatable, Span};
use std::cell::RefCell;
use std::rc::Rc;

impl GlobalValidator {
    pub(super) fn validate_function(
        &mut self,
        func: &Locatable<FunctionDeclaration>,
    ) -> Result<HlirFunction, ()> {
        let (func_span, func) = (func.location, &func.value);
        let (dec_span, dec) = (func.declaration.location, &func.declaration.value);
        if !dec.specifier.specifiers.is_empty() {
            self.report_error(CompilerError::FunctionStorageSpecifiers(dec_span));
            return Err(());
        }

        let ty = self.validate_type(&dec.specifier, dec_span, true)?;
        let ident = &dec.ident;
        if ident.is_none() {
            self.report_error(CompilerError::FunctionRequiresIdentifier(dec_span));
            return Err(());
        }

        let ident = ident.as_ref().unwrap().value.clone();
        let raw_params = &func.parameters;
        let mut parameters = Vec::new();
        for parameter in raw_params {
            parameters.push(self.validate_function_param(parameter)?);
        }

        let param_types = parameters
            .iter()
            .map(|var| var.ty.clone())
            .collect::<Vec<_>>();
        self.scope
            .add_function(&ident, ty.clone(), param_types, func_span);

        self.push_scope();

        for parameter in &parameters {
            let array_size = match &parameter.ty.decl {
                HlirTypeDecl::Array(size) => Some(*size),
                _ => None,
            };
            self.scope
                .add_variable(
                    &parameter.ident,
                    &parameter.ty,
                    parameter.is_const,
                    parameter.initializer.is_some(),
                    array_size,
                    func_span,
                )
                .map_err(|err| {
                    self.report_error(err);
                })?;
        }

        let body = self.validate_block(&func.body)?;

        Ok(HlirFunction {
            ty,
            ident,
            parameters,
            body,
        })
    }
    pub(crate) fn validate_function_param(
        &mut self,
        param: &Locatable<Declaration>,
    ) -> Result<HlirVariable, ()> {
        if !param.specifier.specifiers.is_empty() {
            self.report_error(CompilerError::ParamStorageSpecifiers(param.location));
            return Err(());
        }

        if param.ident.is_none() {
            self.report_error(CompilerError::ParamRequiresIdent(param.location));
            return Err(());
        }

        let hlir_var = self.process_dec_to_hlir_variable(&param.value, param.location)?;

        Ok(hlir_var)
    }

    pub(super) fn validate_variable(
        &mut self,
        locatable_variable: &Locatable<VariableDeclaration>,
    ) -> Result<HlirVariable, ()> {
        let span = locatable_variable.location;
        let var = &locatable_variable.value;

        let declaration = &var.declaration;

        let mut variable =
            self.process_dec_to_hlir_variable(&declaration.value, declaration.location)?;

        let ty = self.validate_type(&declaration.specifier, span, false)?;
        if var.is_array && var.array_size.is_none() && var.initializer.is_none() {
            let err = CompilerError::ArraySizeNotSpecified(span);
            self.report_error(err);
            return Err(());
        }

        let initializer: Option<HlirVarInit> = if let Some(init) = &var.initializer {
            Some(self.validate_initializer(init, init.location)?)
        } else {
            None
        };

        let array_size = var.array_size.map(|size| size as u64).or_else(|| {
            initializer.as_ref().and_then(|init| match init {
                HlirVarInit::Array(arr) => Some(arr.len() as u64),
                HlirVarInit::Expr(_) => None,
            })
        });

        let ty = if let Some(array_size) = array_size {
            let mut ty = ty;
            ty.decl = HlirTypeDecl::Array(array_size);
            ty
        } else {
            ty
        };

        variable.initializer = initializer;
        variable.ty = ty;
        Ok(variable)
    }

    pub(crate) fn process_dec_to_hlir_variable(
        &mut self,
        dec: &Declaration,
        span: Span,
    ) -> Result<HlirVariable, ()> {
        let ident = dec.ident.as_ref().unwrap();
        let ident_span = ident.location;
        let ident = ident.value.clone();

        let mut is_const = false;
        for ty_qual in &dec.specifier.qualifiers {
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
        for storage_spec in &dec.specifier.specifiers {
            // need to change this span to the specific storage spec location
            self.report_warning(CompilerWarning::UnsupportedStorageSpecifier(
                storage_spec.to_string(),
                span,
            ))
        }
        let ty = self.validate_type(&dec.specifier, span, false)?;

        Ok(HlirVariable {
            ty,
            ident,
            is_const,
            initializer: None,
        })
    }

    pub(super) fn validate_initializer(
        &mut self,
        expr: &Expression,
        span: Span,
    ) -> Result<HlirVarInit, ()> {
        if let Expression::ArrayInitializer(arr) = expr {
            let mut inits = Vec::with_capacity(arr.len());
            for init in arr {
                let init = self.validate_expression(expr)?;
                inits.push(init);
            }
            Ok(HlirVarInit::Array(inits))
        } else {
            let expr = self.validate_expression(expr)?;
            Ok(HlirVarInit::Expr(expr))
        }
    }
}
