use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirTypeDecl, MlirVariable, MlirVarInit};

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn compile_variable_declaration(&mut self, var: &'mlir MlirVariable, is_global: bool) {
        let MlirVariable {
            span,
            ty: mlir_type,
            ident,
            is_const,
            initializer,
        } = var;

        let ty = if matches!(&mlir_type.decl, MlirTypeDecl::Array(_)) {
            mlir_type.as_basic()
        } else {
            mlir_type.value.clone()
        };
        let ty = self.convert_type(&ty);

        let var_ptr = match &mlir_type.decl {
            _ if is_global => self
                .module
                .add_global(ty, Some(AddressSpace::default()), ident)
                .as_pointer_value(),
            MlirTypeDecl::Array(size) => self.create_entry_block_array_allocation(ty, *size),
            MlirTypeDecl::Pointer | MlirTypeDecl::Basic => {
                self.create_entry_block_allocation(ty, ident)
            }
        };

        self.insert_pointer(ident.value.clone(), var_ptr);

        let initializer = initializer.as_ref().map(|val| &val.value);

        if is_global {
            self.init_in_main
                .push((ident.value.clone(), ty, initializer));
        } else {
            self.initialize_variable(ty, var_ptr, initializer);
        }
    }

    pub fn initialize_variable(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
        var_ptr: PointerValue<'ctx>,
        initializer: Option<&MlirVarInit>,
    ) {
        if let Some(initializer) = initializer {
            match initializer {
                MlirVarInit::Array(array) => {
                    debug_assert!(ty.is_array_type());
                    self.compile_array_initializer(var_ptr, ty, array);
                }
                MlirVarInit::Expr(expr) => {
                    let expr = self.compile_expression(expr);
                    self.builder().build_store(var_ptr, expr).unwrap();
                }
            }
        }
    }

    fn compile_array_initializer(
        &mut self,
        ptr: PointerValue<'ctx>,
        element_type: BasicTypeEnum<'ctx>,
        array: &[MlirExpr],
    ) {
        let elements = array
            .iter()
            .map(|expr| self.compile_expression(expr))
            .collect::<Vec<_>>();
        for (idx, e) in elements.iter().enumerate() {
            let idx = self.context.i32_type().const_int(idx as u64, false);
            let ptr = self.get_array_index_pointer(element_type, ptr, idx);
            self.builder().build_store(ptr, *e).unwrap();
        }
    }
}
