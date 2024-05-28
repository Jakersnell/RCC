use inkwell::{FloatPredicate, IntPredicate};
use inkwell::builder::{Builder, BuilderError};
use inkwell::values::{BasicValueEnum, IntValue};

use crate::codegen::Compiler;
use crate::data::mlir::MlirExpr;

macro_rules! build_arithmetic_binop {
    (
        $compiler:ident, $left:ident, $right:ident;
        $signed_int_case:ident;
        $float_case:ident;
    ) => {
        build_arithmetic_binop!(
            $compiler, $left, $right;
            $signed_int_case;
            $signed_int_case;
            $float_case;
            false;
        )
    };

    (
        $compiler:ident, $left:ident, $right:ident;
        $signed_int_case:ident;
        $unsigned_int_case:ident;
        $float_case:ident;
        $unsigned_int: expr;
    ) => {
        match $compiler.compile_binary_expr($left, $right) {
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) if !$unsigned_int => {
                BasicValueEnum::from(
                    $compiler
                        .builder()
                        .$signed_int_case(left, right, stringify!($int_case))
                        .unwrap(),
                )
            }

            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) if $unsigned_int => {
                BasicValueEnum::from(
                    $compiler
                        .builder()
                        .$unsigned_int_case(left, right, stringify!($int_case))
                        .unwrap(),
                )
            }

            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                BasicValueEnum::from(
                    $compiler
                        .builder()
                        .$float_case(left, right, stringify!($float_case))
                        .unwrap(),
                )
            }

            unexpected => panic!(
                "Expected (int, int) or (float, float) but found '{:?}'",
                unexpected
            ),
        }
    };
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    #[inline(always)]
    fn compile_binary_expr(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> (BasicValueEnum<'ctx>, BasicValueEnum<'ctx>) {
        (
            self.compile_expression(left),
            self.compile_expression(right),
        )
    }

    pub(super) fn compile_addressof(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = self.compile_expression(expr);
        debug_assert!(matches!(expr, BasicValueEnum::PointerValue(_)));
        expr
    }

    pub(super) fn compile_addition(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        build_arithmetic_binop!(
            self, left, right;
            build_int_add;
            build_float_add;
        )
    }

    pub(super) fn compile_subtraction(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        build_arithmetic_binop!(
            self, left, right;
            build_int_sub;
            build_float_sub;
        )
    }

    pub(super) fn compile_multiplication(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        build_arithmetic_binop!(
            self, left, right;
            build_int_mul;
            build_float_mul;
        )
    }

    pub(super) fn compile_division(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        unsigned_int: bool,
    ) -> BasicValueEnum<'ctx> {
        build_arithmetic_binop!(
            self, left, right;
            build_int_signed_div;
            build_int_unsigned_div;
            build_float_div;
            unsigned_int;
        )
    }

    pub(super) fn compile_modulus(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        unsigned_int: bool,
    ) -> BasicValueEnum<'ctx> {
        build_arithmetic_binop!(
            self, left, right;
            build_int_signed_rem;
            build_int_unsigned_rem;
            build_float_rem;
            unsigned_int;
        )
    }

    fn build_comparison(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        int_predicate: IntPredicate,
        float_predicate: FloatPredicate,
    ) -> BasicValueEnum<'ctx> {
        match self.compile_binary_expr(left, right) {
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                BasicValueEnum::from(
                    self.builder()
                        .build_int_compare(int_predicate, left, right, "int_eq")
                        .unwrap(),
                )
            }
            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                BasicValueEnum::from(
                    self.builder()
                        .build_float_compare(float_predicate, left, right, "float_eq")
                        .unwrap(),
                )
            }
            unexpected => panic!(
                "Expected (int, int) or (float, float) but found '{:?}'",
                unexpected
            ),
        }
    }

    #[inline(always)]
    pub(super) fn compile_equal(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.build_comparison(left, right, IntPredicate::EQ, FloatPredicate::OEQ)
    }

    #[inline(always)]
    pub(super) fn compile_not_equal(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.build_comparison(left, right, IntPredicate::NE, FloatPredicate::ONE)
    }

    #[inline(always)]
    pub(super) fn compile_greater_than(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        unsigned_int: bool,
    ) -> BasicValueEnum<'ctx> {
        self.build_comparison(
            left,
            right,
            if unsigned_int {
                IntPredicate::UGT
            } else {
                IntPredicate::SGT
            },
            FloatPredicate::OGT,
        )
    }

    #[inline(always)]
    pub(super) fn compile_greater_than_equal(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        unsigned_int: bool,
    ) -> BasicValueEnum<'ctx> {
        self.build_comparison(
            left,
            right,
            if unsigned_int {
                IntPredicate::UGE
            } else {
                IntPredicate::SGE
            },
            FloatPredicate::OGE,
        )
    }

    #[inline(always)]
    pub(super) fn compile_less_than(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        unsigned_int: bool,
    ) -> BasicValueEnum<'ctx> {
        self.build_comparison(
            left,
            right,
            if unsigned_int {
                IntPredicate::ULT
            } else {
                IntPredicate::SLT
            },
            FloatPredicate::OLT,
        )
    }

    #[inline(always)]
    pub(super) fn compile_less_than_equal(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        unsigned_int: bool,
    ) -> BasicValueEnum<'ctx> {
        self.build_comparison(
            left,
            right,
            if unsigned_int {
                IntPredicate::ULE
            } else {
                IntPredicate::SLE
            },
            FloatPredicate::OLE,
        )
    }

    fn compile_binary_logical_expr(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let left = self.compile_expression(left).into_int_value();
        let right = self.compile_expression(right).into_int_value();
        let zero = self.context.i8_type().const_int(0, false);

        let logical_left = self
            .builder()
            .build_int_compare(IntPredicate::NE, left, zero, "logical_cmp_left")
            .unwrap();

        let logical_right = self
            .builder()
            .build_int_compare(IntPredicate::NE, right, zero, "logical_cmp_right")
            .unwrap();

        (logical_left, logical_right)
    }

    fn compile_int_binop<BuildClosure>(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        build_closure: BuildClosure,
    ) -> BasicValueEnum<'ctx>
    where
        BuildClosure: Fn(
            &Builder<'ctx>,
            IntValue<'ctx>,
            IntValue<'ctx>,
        ) -> Result<IntValue<'ctx>, BuilderError>,
    {
        let left = self.compile_expression(left).into_int_value();
        let right = self.compile_expression(right).into_int_value();
        let value = build_closure(self.builder(), left, right).unwrap();
        BasicValueEnum::from(value)
    }

    #[inline(always)]
    pub(super) fn compile_logical_and(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_and(left, right, "logical_and")
        })
    }

    #[inline(always)]
    pub(super) fn compile_logical_or(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_or(left, right, "logical_or")
        })
    }

    #[inline(always)]
    pub(super) fn compile_bitwise_and(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_and(left, right, "bitwise_and")
        })
    }

    #[inline(always)]
    pub(super) fn compile_bitwise_or(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_or(left, right, "bitwise_or")
        })
    }

    #[inline(always)]
    pub(super) fn compile_bitwise_xor(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_xor(left, right, "bitwise_xor")
        })
    }

    #[inline(always)]
    pub(super) fn compile_left_shift(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_left_shift(left, right, "left_shift")
        })
    }

    #[inline(always)]
    pub(super) fn compile_right_shift(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_right_shift(left, right, false, "right_shift")
        })
    }
}
