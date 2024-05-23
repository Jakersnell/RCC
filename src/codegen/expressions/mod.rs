use inkwell::{FloatPredicate, IntPredicate};
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirExprKind, MlirLiteral};

mod binary_expressions;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub(super) fn compile_expression(&mut self, expression: &MlirExpr) -> BasicValueEnum<'ctx> {
        match &*expression.kind {
            MlirExprKind::Literal(literal) => self.compile_literal(literal),
            MlirExprKind::Variable(id) => self.compile_variable_access(*id),
            MlirExprKind::PostIncrement(expr) => self.compile_post_increment(expr),
            MlirExprKind::PostDecrement(expr) => self.compile_post_decrement(expr),
            MlirExprKind::Negate(expr) => self.compile_negate(expr),
            MlirExprKind::LogicalNot(expr) => self.compile_logical_not(expr),
            MlirExprKind::BitwiseNot(expr) => self.compile_bitwise_not(expr),
            MlirExprKind::Deref(expr) => self.compile_deref(expr),
            MlirExprKind::Assign(left, right) => todo!(), // left can be Deref, Var, or Member
            MlirExprKind::AddressOf(_) => todo!(),
            MlirExprKind::Add(_, _) => todo!(),
            MlirExprKind::Sub(_, _) => todo!(),
            MlirExprKind::Mul(_, _) => todo!(),
            MlirExprKind::Div(_, _) => todo!(),
            MlirExprKind::Mod(_, _) => todo!(),
            MlirExprKind::Equal(_, _) => todo!(),
            MlirExprKind::NotEqual(_, _) => todo!(),
            MlirExprKind::GreaterThan(_, _) => todo!(),
            MlirExprKind::GreaterThanEqual(_, _) => todo!(),
            MlirExprKind::LessThan(_, _) => todo!(),
            MlirExprKind::LessThanEqual(_, _) => todo!(),
            MlirExprKind::LogicalAnd(_, _) => todo!(),
            MlirExprKind::LogicalOr(_, _) => todo!(),
            MlirExprKind::BitwiseAnd(_, _) => todo!(),
            MlirExprKind::BitwiseOr(_, _) => todo!(),
            MlirExprKind::BitwiseXor(_, _) => todo!(),
            MlirExprKind::LeftShift(_, _) => todo!(),
            MlirExprKind::RightShift(_, _) => todo!(),
            MlirExprKind::FunctionCall { .. } => todo!(),
            MlirExprKind::Index(_, _) => todo!(),
            MlirExprKind::Member(_, _) => todo!(),
            MlirExprKind::Cast(_, _) => todo!(),
        }
    }

    #[rustfmt::skip]
    fn compile_literal(&self, literal: &MlirLiteral) -> BasicValueEnum<'ctx> {
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
            MlirLiteral::String(bytes) => {
                let bytes = bytes
                    .iter()
                    .map(|byte| {
                        self.context.i8_type().const_int(*byte as u64, false)
                    })
                    .collect::<Vec<_>>();
                self.context.i8_type().const_array(&bytes).into()
            }
        }
    }

    fn compile_variable_access(&mut self, id: usize) -> BasicValueEnum<'ctx> {
        BasicValueEnum::from(
            *self
                .variables
                .get(&id)
                .expect("Invalid variable accesses must be handled in Analysis."),
        )
    }

    fn compile_post_increment(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_post_inc_or_dec(expr, true)
    }

    fn compile_post_decrement(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_post_inc_or_dec(expr, false)
    }

    fn compile_post_inc_or_dec(&mut self, expr: &MlirExpr, inc: bool) -> BasicValueEnum<'ctx> {
        debug_assert!(expr.is_lval);
        let expr = self.compile_expression(expr);

        let ptr = match expr {
            BasicValueEnum::PointerValue(ptr) => ptr,
            _ => panic!("Expected BasicValueEnum::PointerValue, found '{:?}'", expr),
        };

        let ptr_type = self.get_pointer_type(&ptr);
        let ptr_value = self
            .builder()
            .build_load(ptr_type, ptr, "post_inc_load_value")
            .unwrap();

        match ptr_value {
            BasicValueEnum::IntValue(int) => self.compile_post_inc_or_dec_for_int(ptr, int, inc),
            BasicValueEnum::FloatValue(float) => {
                self.compile_post_inc_or_dec_for_float(ptr, float, inc)
            }
            BasicValueEnum::PointerValue(inner_ptr) => {
                self.compile_post_inc_or_dec_for_ptr(ptr, inner_ptr, inc)
            }
            _ => unreachable!(),
        }
    }

    fn compile_post_inc_or_dec_for_int(
        &self,
        ptr: PointerValue<'ctx>,
        int: IntValue<'ctx>,
        inc: bool,
    ) -> BasicValueEnum<'ctx> {
        let one = self.context.i8_type().const_int(1, false);

        let new_value = if inc {
            self.builder().build_int_add(int, one, "int_post_inc_add")
        } else {
            self.builder().build_int_sub(int, one, "int_post_inc_sub")
        }
        .unwrap();

        self.builder().build_store(ptr, new_value).unwrap();

        BasicValueEnum::from(int)
    }

    fn compile_post_inc_or_dec_for_float(
        &self,
        ptr: PointerValue<'ctx>,
        float: FloatValue<'ctx>,
        inc: bool,
    ) -> BasicValueEnum<'ctx> {
        let one = self.context.f32_type().const_float(1.0);

        let new_value = if inc {
            self.builder()
                .build_float_add(float, one, "float_post_inc_add")
        } else {
            self.builder()
                .build_float_sub(float, one, "float_post_inc_sub")
        }
        .unwrap();

        self.builder().build_store(ptr, new_value).unwrap();

        BasicValueEnum::from(float)
    }

    fn compile_post_inc_or_dec_for_ptr(
        &self,
        ptr: PointerValue<'ctx>,
        inner_ptr: PointerValue<'ctx>,
        inc: bool,
    ) -> BasicValueEnum<'ctx> {
        let ptr_to_int = self
            .builder()
            .build_ptr_to_int(inner_ptr, self.context.i64_type(), "ptr_to_int")
            .unwrap();

        let one = self.context.i8_type().const_int(1, false);
        let new_int_value = if inc {
            self.builder()
                .build_int_add(ptr_to_int, one, "ptr_post_inc_add")
        } else {
            self.builder()
                .build_int_sub(ptr_to_int, one, "ptr_post_inc_sub")
        }
        .unwrap();

        let int_to_ptr = self
            .builder()
            .build_int_to_ptr(new_int_value, inner_ptr.get_type(), "int_to_ptr")
            .unwrap();

        self.builder().build_store(ptr, int_to_ptr).unwrap();

        BasicValueEnum::from(inner_ptr)
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
        let int_val = match self.compile_expression(expr) {
            BasicValueEnum::IntValue(int_val) => int_val,
            _ => panic!(),
        };

        BasicValueEnum::from(self.builder().build_not(int_val, "bitwise_not").unwrap())
    }

    fn compile_deref(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = match self.compile_expression(expr) {
            BasicValueEnum::PointerValue(ptr) => ptr,
            unexpected => panic!(
                "Expected PointerValue, but found '{:?}'",
                unexpected.get_type()
            ),
        };
        self.deref_pointer_value(expr)
    }

    fn compile_address_of(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        // address of should only be taken on a lval which when compiled returns a pointer
        let expr = self.compile_expression(expr);
        if matches!(expr, BasicValueEnum::PointerValue(_)) {
            expr
        } else {
            panic!()
        }
    }
}
