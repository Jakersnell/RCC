use inkwell::{FloatPredicate, IntPredicate};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

use crate::codegen::Compiler;
use crate::data::mlir::{CastType, MlirExpr, MlirExprKind, MlirType};
use crate::util::str_intern::InternedStr;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn compile_expression(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        match &*expr.kind {
            MlirExprKind::Literal(literal) => self.compile_literal(literal),
            MlirExprKind::Variable(id) => self.compile_variable_access(&expr.ty, id),
            MlirExprKind::PostIncrement(expr) => self.compile_post_increment(expr),
            MlirExprKind::PostDecrement(expr) => self.compile_post_decrement(expr),
            MlirExprKind::Negate(expr) => self.compile_negate(expr),
            MlirExprKind::LogicalNot(expr) => self.compile_logical_not(expr),
            MlirExprKind::BitwiseNot(expr) => self.compile_bitwise_not(expr),
            MlirExprKind::Deref(expr) => self.compile_deref(expr),
            MlirExprKind::AddressOf(expr) => self.compile_address_of(expr),
            MlirExprKind::Assign(left, right) => self.compile_assignment(left, right, false),
            MlirExprKind::Add(left, right) => self.compile_addition(left, right),
            MlirExprKind::Sub(left, right) => self.compile_subtraction(left, right),
            MlirExprKind::Mul(left, right) => self.compile_multiplication(left, right),
            MlirExprKind::Div(left, right) => {
                self.compile_division(left, right, expr.ty.is_unsigned_int())
            }
            MlirExprKind::Mod(left, right) => {
                self.compile_modulus(left, right, expr.ty.is_unsigned_int())
            }
            MlirExprKind::Equal(left, right) => self.compile_equal(left, right),
            MlirExprKind::NotEqual(left, right) => self.compile_not_equal(left, right),
            MlirExprKind::GreaterThan(left, right) => {
                self.compile_greater_than(left, right, expr.ty.is_unsigned_int())
            }
            MlirExprKind::GreaterThanEqual(left, right) => {
                self.compile_greater_than_equal(left, right, expr.ty.is_unsigned_int())
            }
            MlirExprKind::LessThan(left, right) => {
                self.compile_less_than(left, right, expr.ty.is_unsigned_int())
            }
            MlirExprKind::LessThanEqual(left, right) => {
                self.compile_less_than_equal(left, right, expr.ty.is_unsigned_int())
            }
            MlirExprKind::LogicalAnd(left, right) => self.compile_logical_and(left, right),
            MlirExprKind::LogicalOr(left, right) => self.compile_logical_or(left, right),
            MlirExprKind::BitwiseAnd(left, right) => self.compile_bitwise_and(left, right),
            MlirExprKind::BitwiseOr(left, right) => self.compile_bitwise_or(left, right),
            MlirExprKind::BitwiseXor(left, right) => self.compile_bitwise_xor(left, right),
            MlirExprKind::LeftShift(left, right) => self.compile_left_shift(left, right),
            MlirExprKind::RightShift(left, right) => self.compile_right_shift(left, right),
            MlirExprKind::Index(array, index) => self.compile_array_access(array, index, &expr.ty),
            MlirExprKind::Member(_struct, member) => self.compile_member_access(_struct, member),
            MlirExprKind::Cast(cast_type, expr) => self.compile_cast(cast_type, expr, &expr.ty),
            MlirExprKind::FunctionCall {
                location,
                ident,
                args,
            } => self.compile_function_call(location, ident, args),
        }
    }

    fn compile_function_call(
        &mut self,
        location: &Option<&str>,
        ident: &InternedStr,
        args: &[MlirExpr],
    ) -> BasicValueEnum<'ctx> {
        let function = *self
            .functions
            .get(ident)
            .expect("Function does not exist in memory.");
        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .map(|arg| self.compile_expression(arg).into())
            .collect();
        let call_site_value = self
            .builder()
            .build_call(function, &compiled_args, "function_call")
            .unwrap();
        // hack to preserve returning VOID without having to return Some(BasicValueEnum<'ctx> at every method
        let const_i8 = BasicValueEnum::from(self.context.i8_type().const_int(0, false));
        call_site_value.try_as_basic_value().left_or(const_i8)
    }

    fn compile_cast(
        &mut self,
        cast_type: &CastType,
        expr: &MlirExpr,
        mlir_type: &MlirType,
    ) -> BasicValueEnum<'ctx> {
        let unsigned_int = expr.ty.is_unsigned_int() || mlir_type.is_unsigned_int();
        let expr = self.compile_expression(expr);

        if matches!(
            cast_type,
            CastType::SignedToUnsigned
                | CastType::UnsignedToSigned
                | CastType::PointerToPointer
                | CastType::ArrayToPointer
        ) {
            // compiler usage casts, no corresponding runtime cast
            return expr;
        }

        match cast_type {
            CastType::PointerToLong => {
                let i64_type = self.context.i64_type();
                let ptr = expr.into_pointer_value();
                let value = self
                    .builder()
                    .build_ptr_to_int(ptr, i64_type, "ptr_to_int")
                    .unwrap();
                BasicValueEnum::from(value)
            }

            CastType::LongToPointer => {
                let ptr_type = self.convert_type(mlir_type).into_pointer_type();
                let long = expr.into_int_value();
                let value = self
                    .builder()
                    .build_int_to_ptr(long, ptr_type, "int_to_ptr")
                    .unwrap();
                BasicValueEnum::from(value)
            }

            CastType::IntToLong
            | CastType::LongToInt
            | CastType::IntToChar
            | CastType::CharToInt => {
                let int = expr.into_int_value();
                let int_type = self.convert_type(mlir_type).into_int_type();
                let value = self
                    .builder()
                    .build_int_cast(int, int_type, "int_to_int")
                    .unwrap();
                BasicValueEnum::from(value)
            }

            CastType::IntToFloat | CastType::LongToDouble => {
                let int = expr.into_int_value();
                let float_type = self.convert_type(mlir_type).into_float_type();
                let value = if unsigned_int {
                    self.builder().build_unsigned_int_to_float(
                        int,
                        float_type,
                        "unsigned_int_to_float",
                    )
                } else {
                    self.builder()
                        .build_signed_int_to_float(int, float_type, "signed_int_to_float")
                }
                .unwrap();
                BasicValueEnum::from(value)
            }

            CastType::DoubleToLong | CastType::FloatToInt => {
                let float = expr.into_float_value();
                let int_type = self.convert_type(mlir_type).into_int_type();
                let value = if unsigned_int {
                    self.builder().build_float_to_unsigned_int(
                        float,
                        int_type,
                        "float_to_unsigned_int",
                    )
                } else {
                    self.builder()
                        .build_float_to_signed_int(float, int_type, "float_to_signed_int")
                }
                .unwrap();
                BasicValueEnum::from(value)
            }

            CastType::FloatToDouble | CastType::DoubleToFloat => {
                let float = expr.into_float_value();
                let float_type = self.convert_type(mlir_type).into_float_type();
                let value = self
                    .builder()
                    .build_float_cast(float, float_type, "float_to_float")
                    .unwrap();
                BasicValueEnum::from(value)
            }

            _ => unreachable!(),
        }
    }

    fn compile_negate(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = self.compile_expression(expr);
        match expr {
            BasicValueEnum::IntValue(int) => {
                BasicValueEnum::from(self.builder().build_int_neg(int, "int-neg").unwrap())
            }
            BasicValueEnum::FloatValue(float) => {
                BasicValueEnum::from(self.builder().build_float_neg(float, "float-neg").unwrap())
            }
            _ => panic!("Value cannot be negated: {:?}", expr),
        }
    }

    fn compile_logical_not(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = self.compile_expression(expr);
        let int_val = match expr {
            BasicValueEnum::IntValue(int_val) => {
                let zero = self.context.i8_type().const_int(0, false);
                self.builder()
                    .build_int_compare(IntPredicate::EQ, int_val, zero, "int_cmp_zero_logical_not")
                    .unwrap()
            }

            BasicValueEnum::FloatValue(float_val) => {
                let zero = self.context.f32_type().const_float(0.0);
                self.builder()
                    .build_float_compare(
                        FloatPredicate::OEQ,
                        float_val,
                        zero,
                        "float_cmp_zero_logical_not",
                    )
                    .unwrap()
            }
            _ => panic!(),
        };

        BasicValueEnum::from(int_val)
    }

    fn compile_bitwise_not(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let int_val = self.compile_expression(expr).into_int_value();
        BasicValueEnum::from(self.builder().build_not(int_val, "bitwise_not").unwrap())
    }
}
