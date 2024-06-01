use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirTypeDecl, MlirVariable, MlirVarInit};

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn compile_variable_declaration(&mut self, var: &MlirVariable) {
        let MlirVariable {
            uid,
            span,
            ty: mlir_type,
            ident,
            is_const,
            initializer,
        } = var;

        let (ty, var_ptr) = match &mlir_type.decl {
            MlirTypeDecl::Array(size) => {
                let element_type = self.convert_type(&mlir_type.as_basic());
                (
                    element_type,
                    self.create_entry_block_array_allocation(element_type, *size),
                )
            }
            _ => {
                let ty = self.convert_type(mlir_type);
                (ty, self.create_entry_block_allocation(ty, ident))
            }
        };

        self.variables.insert(*uid, var_ptr).unwrap();

        if let Some(initializer) = initializer {
            match &initializer.value {
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
