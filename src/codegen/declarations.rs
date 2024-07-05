use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirTypeDecl, MlirVariable, MlirVarInit};

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn compile_global_variable_declaration(&mut self, var: &'mlir MlirVariable) {
        let MlirVariable {
            uid,
            span,
            ty: mlir_type,
            ident,
            is_const,
            initializer,
        } = var;

        let mut ty = self.convert_type(&if matches!(&mlir_type.decl, MlirTypeDecl::Array(_)) {
            mlir_type.as_basic()
        } else {
            mlir_type.value.clone()
        });

        let global = match &mlir_type.decl {
            MlirTypeDecl::Array(size) => {
                ty = ty.array_type(*size as u32).into();
                self.module.add_global(ty, None, ident)
            }
            _ => self.module.add_global(ty, None, ident),
        };

        global.set_initializer(&self.create_default_value_for_type(ty));

        self.insert_pointer(*uid, global.as_pointer_value());

        let initializer = initializer.as_ref().map(|val| &val.value);

        self.init_in_main.push((*uid, ty, initializer));
    }

    fn create_default_value_for_type(&mut self, ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let basic_value = match ty {
            BasicTypeEnum::ArrayType(array_type) => array_type.const_zero().into(),
            BasicTypeEnum::StructType(struct_type) => struct_type.const_zero().into(),
            BasicTypeEnum::FloatType(float_type) => float_type.const_float(0.0).into(),
            BasicTypeEnum::IntType(int_type) => int_type.const_int(0, false).into(),
            BasicTypeEnum::PointerType(ptr_type) => ptr_type.const_null().into(),
            _ => unreachable!(),
        };

        basic_value
    }

    pub fn compile_variable_declaration(&mut self, var: &'mlir MlirVariable) {
        let MlirVariable {
            uid,
            span,
            ty: mlir_type,
            ident,
            is_const,
            initializer,
        } = var;

        let ty = self.convert_type(&if matches!(&mlir_type.decl, MlirTypeDecl::Array(_)) {
            mlir_type.as_basic()
        } else {
            mlir_type.value.clone()
        });

        let var_ptr = match &mlir_type.decl {
            MlirTypeDecl::Array(size) => self.create_entry_block_array_allocation(ty, *size),
            MlirTypeDecl::Pointer | MlirTypeDecl::Basic => {
                self.create_entry_block_allocation(ty, ident)
            }
        };

        self.insert_pointer(*uid, var_ptr);

        let initializer = initializer.as_ref().map(|val| &val.value);

        self.initialize_variable(ty, var_ptr, initializer);
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
