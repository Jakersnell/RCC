use inkwell::values::BasicValueEnum;

use crate::codegen::Compiler;
use crate::data::mlir::MlirLiteral;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    #[rustfmt::skip]
    pub fn compile_literal(&mut self, literal: &MlirLiteral) -> BasicValueEnum<'ctx> {
        match literal {
            MlirLiteral::Char(char) => {
                self.context.i8_type().const_int(*char as u64, true).into()
            }
            MlirLiteral::UChar(char) => {
                self.context.i8_type().const_int(*char as u64, false).into()
            }
            MlirLiteral::Int(int) => {
                self.context.i32_type().const_int(*int as u64, true).into()
            }
            MlirLiteral::UInt(int) => {
                self.context.i32_type().const_int(*int as u64, false).into()
            }
            MlirLiteral::Long(long) => {
                self.context.i64_type().const_int(*long as u64, true).into()
            }
            MlirLiteral::ULong(long) => {
                self.context.i64_type().const_int(*long, false).into()
            }
            MlirLiteral::Float(float) => {
                self.context.f32_type().const_float(*float as f64).into()
            }
            MlirLiteral::Double(double) => {
                self.context.f64_type().const_float(*double).into()
            }
            MlirLiteral::String(string) => {
                let string_type = self.context.i8_type().array_type(string.len() as u32 + 1); // add one for null term
                let global = self.module.add_global(string_type, None, "global_string");
                global.set_initializer(&self.context.const_string(string, true));
                global.as_pointer_value().into()
            }
        }
    }
}
