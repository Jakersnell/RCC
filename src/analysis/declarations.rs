use crate::analysis::{Analyzer, control_flow};
use crate::data::ast::*;
use crate::data::mlir::*;
use crate::util::{Locatable, Span};
use crate::util::error::{CompilerError, CompilerWarning};

impl Analyzer {
    pub(super) fn validate_struct_definition(
        &mut self,
        _struct: &Locatable<StructDeclaration>,
    ) -> Result<MlirStruct, ()> {
        if _struct.declaration.specifier.pointer {
            self.report_error(CompilerError::StructDeclarationPointer(_struct.location));
        }
        if !_struct.declaration.specifier.qualifiers.is_empty() {
            self.report_error(CompilerError::StructDeclarationQualifiers(_struct.location));
        }
        if !_struct.declaration.specifier.specifiers.is_empty() {
            self.report_error(CompilerError::StructStorageSpecifiers(_struct.location));
        }
        let as_ty = self.validate_type(
            &_struct.declaration.specifier,
            _struct.declaration.location,
            false,
            true,
        )?;
        let ident = match &as_ty.kind {
            MlirTypeKind::Struct(ident) => ident.clone(),
            _ => panic!(),
        };
        let ident = _struct.declaration.location.into_locatable(ident);
        let location = _struct.location;
        let mut fields = Vec::new();
        let mut size = 0;
        for member in &_struct.members {
            let span = member.location;
            let member = self.process_dec_to_hlir_variable(member, span)?;
            size += self.sizeof(&member.ty, span);
            fields.push(span.into_locatable(member));
        }
        let _struct = MlirStruct {
            ident,
            fields,
            size,
        };
        let add_struct_result = self
            .scope
            .borrow_mut()
            .add_struct(as_ty, &_struct, location);
        if let Err(err) = add_struct_result {
            self.report_error(err);
        }
        Ok(_struct)
    }

    pub(super) fn validate_function_definition(
        &mut self,
        func: &Locatable<FunctionDeclaration>,
    ) -> Result<MlirFunction, ()> {
        let (func_span, func) = (func.location, &func.value);
        let (dec_span, dec) = (func.declaration.location, &func.declaration.value);
        if !dec.specifier.specifiers.is_empty() {
            self.report_error(CompilerError::FunctionStorageSpecifiers(dec_span));
            return Err(());
        }

        let ty =
            dec_span.into_locatable(self.validate_type(&dec.specifier, dec_span, true, false)?);

        self.return_ty = Some(ty.clone());

        let ident = &dec.ident;
        if ident.is_none() {
            self.report_error(CompilerError::FunctionRequiresIdentifier(dec_span));
            return Err(());
        }
        let ident = ident.as_ref().unwrap();
        let ident = ident.location.into_locatable(ident.value.clone());

        let raw_params = &func.parameters;
        let mut parameters = Vec::new();
        for parameter in raw_params {
            parameters.push(
                parameter
                    .location
                    .into_locatable(self.validate_function_param_declaration(parameter)?),
            );
        }

        let param_types = parameters
            .iter()
            .map(|var| var.ty.clone())
            .collect::<Vec<_>>();
        self.scope
            .borrow_mut()
            .add_function(&ident, ty.clone(), param_types, func_span);

        self.push_scope();
        for parameter in &parameters {
            self.add_variable_to_scope(parameter, func_span);
        }

        let body = self.validate_block(&func.body);
        self.pop_scope();
        let mut body = Self::flatten_blocks(body?);
        if *self.return_ty.as_ref().unwrap() == VOID_TYPE {
            let return_void = MlirStmt::Return(None);
            body.0.push(return_void);
        }
        let body = func.body.location.into_locatable(body);
        self.return_ty = None;
        let func = MlirFunction {
            span: func_span,
            ty,
            ident,
            parameters,
            body,
        };
        self.validate_function_return(&func, func_span);
        Ok(func)
    }

    fn validate_function_return(&mut self, function: &MlirFunction, span: Span) {
        let cfg = control_flow::ControlFlowGraph::new(&function.body, &format!("<fn {}; {}>", function.ident.value, function.span));
        if !cfg.all_paths_return() {
            self.report_error(CompilerError::FunctionMissingReturn(
                function.ident.to_string(),
                span,
            ));
        }
    }

    fn flatten_blocks(hlir_block: MlirBlock) -> MlirBlock {
        let mut block = Vec::new();
        Self::flatten_blocks_recursive(hlir_block, &mut block);
        MlirBlock(block)
    }

    fn flatten_blocks_recursive(hlir_block: MlirBlock, vec: &mut Vec<MlirStmt>) {
        for stmt in hlir_block.0 {
            match stmt {
                MlirStmt::Block(inner_block) => Self::flatten_blocks_recursive(inner_block, vec),
                other => vec.push(other),
            }
        }
    }

    pub(crate) fn validate_function_param_declaration(
        &mut self,
        param: &Locatable<Declaration>,
    ) -> Result<MlirVariable, ()> {
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

    pub(super) fn validate_variable_declaration(
        &mut self,
        locatable_variable: &Locatable<VariableDeclaration>,
    ) -> Result<MlirVariable, ()> {
        let span = locatable_variable.location;
        let var = &locatable_variable.value;

        let declaration = &var.declaration;

        let mut variable =
            self.process_dec_to_hlir_variable(&declaration.value, declaration.location)?;

        let ty = self.validate_type(&declaration.specifier, span, false, false)?;
        if var.is_array && var.array_size.is_none() && var.initializer.is_none() {
            let err = CompilerError::ArraySizeNotSpecified(span);
            self.report_error(err);
            return Err(());
        }

        let initializer = if let Some(init) = &var.initializer {
            Some(
                init.location
                    .into_locatable(self.validate_initializer(init, init.location)?),
            )
        } else {
            None
        };

        let array_size = var.array_size.map(|size| size as u64).or_else(|| {
            initializer.as_ref().and_then(|init| match &init.value {
                MlirVarInit::Array(arr) => Some(arr.len() as u64),
                MlirVarInit::Expr(_) => None,
            })
        });

        let ty = if let Some(array_size) = array_size {
            let mut ty = ty;
            ty.decl = MlirTypeDecl::Array(array_size);
            ty
        } else {
            ty
        };

        let ty = declaration.location.into_locatable(ty);

        variable.initializer = initializer;
        variable.ty = ty;

        Ok(variable)
    }

    pub(crate) fn process_dec_to_hlir_variable(
        &mut self,
        dec: &Declaration,
        span: Span,
    ) -> Result<MlirVariable, ()> {
        let ident = dec.ident.as_ref().unwrap();
        let ident_span = ident.location;
        let ident = ident.location.into_locatable(ident.value.clone());

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
        let ty = span.into_locatable(self.validate_type(&dec.specifier, span, false, false)?);

        Ok(MlirVariable {
            span,
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
    ) -> Result<MlirVarInit, ()> {
        if let Expression::ArrayInitializer(arr) = expr {
            let mut inits = Vec::with_capacity(arr.len());
            for init in arr {
                let init = self.validate_expression(init)?;
                inits.push(init);
            }
            Ok(MlirVarInit::Array(inits))
        } else {
            let expr = self.validate_expression(expr)?;
            Ok(MlirVarInit::Expr(expr))
        }
    }
}
