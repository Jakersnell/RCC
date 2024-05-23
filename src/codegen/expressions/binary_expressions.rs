use inkwell::values::BasicValueEnum;

use crate::codegen::Compiler;
use crate::data::mlir::MlirExpr;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub(super) fn compile_assignment(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        todo!()
    }
}
