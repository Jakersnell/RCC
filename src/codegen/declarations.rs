use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirVarInit, MlirVariable};

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub(super) fn compile_variable_declaration(&mut self, var: &MlirVariable) {
        let MlirVariable {
            uid,
            span,
            ty,
            ident,
            is_const,
            initializer,
        } = var;
        let ty = self.convert_type(ty);
        let var_ptr = self.create_entry_block_allocation(ty, &uid.to_string());
        self.variables.insert(*uid, var_ptr).unwrap();

        let initializer = match initializer.as_ref().map(|init| &init.value) {
            Some(MlirVarInit::Array(array)) => self.compile_array_initializer(ty, array),
            Some(MlirVarInit::Expr(expr)) => self.compile_expression(expr),
            None => todo!(),
        };
    }

    fn compile_array_initializer(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
        array: &[MlirExpr],
    ) -> BasicValueEnum<'ctx> {
        let array = array
            .iter()
            .map(|expr| self.compile_expression(expr))
            .collect::<Vec<_>>();
        let count = array.len();

        todo!()
    }

    fn create_array_allocation(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
        size: usize,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        let array_type = ty.array_type(size as u32);
        todo!()
    }

    fn create_basic_default_initializer(
        &mut self,
        ty: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
