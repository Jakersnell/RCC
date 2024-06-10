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

    fn compile_addition_or_subtraction(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        is_addition: bool,
    ) -> BasicValueEnum<'ctx> {
        match self.compile_binary_expr(left, right) {
            (BasicValueEnum::PointerValue(left_val), BasicValueEnum::IntValue(right_val)) => {
                let ptr_type = self.convert_type(&left.ty).into_pointer_type();
                let i64_type = self.context.i64_type();

                let ptr_to_int = self
                    .builder()
                    .build_ptr_to_int(left_val, i64_type, "ptr_to_int")
                    .unwrap();

                let ptr_as_int = if is_addition {
                    self.builder()
                        .build_int_add(ptr_to_int, right_val, "inc_ptr_as_int")
                } else {
                    self.builder()
                        .build_int_sub(ptr_to_int, right_val, "dec_ptr_as_int")
                }
                .unwrap();

                let int_to_ptr = self
                    .builder()
                    .build_int_to_ptr(ptr_as_int, ptr_type, "int_to_ptr")
                    .unwrap();

                BasicValueEnum::from(int_to_ptr)
            }

            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                BasicValueEnum::from(if is_addition {
                    self.builder()
                        .build_int_add(left, right, "int_add")
                        .unwrap()
                } else {
                    self.builder()
                        .build_int_sub(left, right, "int_sub")
                        .unwrap()
                })
            }

            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                BasicValueEnum::from(if is_addition {
                    self.builder()
                        .build_float_add(left, right, "float_add")
                        .unwrap()
                } else {
                    self.builder()
                        .build_float_sub(left, right, "float_sub")
                        .unwrap()
                })
            }

            unexpected => panic!(
                "Expected (ptr, int), (int, int) or (float, float) but found '{:?}'",
                unexpected
            ),
        }
    }

    #[inline(always)]
    pub fn compile_addition(&mut self, left: &MlirExpr, right: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_addition_or_subtraction(left, right, true)
    }

    #[inline(always)]
    pub fn compile_subtraction(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_addition_or_subtraction(left, right, false)
    }

    pub fn compile_multiplication(
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

    pub fn compile_division(
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

    pub fn compile_modulus(
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
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        if cfg!(debug_assertions) && left.ty != right.ty {
            println!("LEFT TYPE:\n{:#?}", left.ty);
            println!("RIGHT TYPE:\n{:#?}", right.ty);
            panic!("Left and Right CMP types are not the same.")
        }

        macro_rules! tag_name {
            () => {
                &format!("cmp_{name}")
            };
        }
        match self.compile_binary_expr(left, right) {
            (BasicValueEnum::PointerValue(left), BasicValueEnum::PointerValue(right)) => {
                BasicValueEnum::from(
                    self.builder()
                        .build_int_compare(int_predicate, left, right, tag_name!())
                        .unwrap(),
                )
            }
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                BasicValueEnum::from(
                    self.builder()
                        .build_int_compare(int_predicate, left, right, tag_name!())
                        .unwrap(),
                )
            }
            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                BasicValueEnum::from(
                    self.builder()
                        .build_float_compare(float_predicate, left, right, tag_name!())
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
    pub fn compile_equal(&mut self, left: &MlirExpr, right: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.build_comparison(left, right, IntPredicate::EQ, FloatPredicate::OEQ, "equal")
    }

    #[inline(always)]
    pub fn compile_not_equal(&mut self, left: &MlirExpr, right: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.build_comparison(
            left,
            right,
            IntPredicate::NE,
            FloatPredicate::ONE,
            "not_equal",
        )
    }

    #[inline(always)]
    pub fn compile_greater_than(
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
            "greater_than",
        )
    }

    #[inline(always)]
    pub fn compile_greater_than_equal(
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
            "greater_than_equal",
        )
    }

    #[inline(always)]
    pub fn compile_less_than(
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
            "less_than",
        )
    }

    #[inline(always)]
    pub fn compile_less_than_equal(
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
            "less_than_equal",
        )
    }

    fn compile_logical_binop<BuildClosure>(
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
        let value = match self.compile_binary_expr(left, right) {
            (BasicValueEnum::PointerValue(left), BasicValueEnum::PointerValue(right)) => {
                let i64_type = self.context.i64_type();
                let left = self
                    .builder()
                    .build_ptr_to_int(left, i64_type, "ptr_to_int")
                    .unwrap();
                let right = self
                    .builder()
                    .build_ptr_to_int(right, i64_type, "ptr_to_int")
                    .unwrap();
                build_closure(self.builder(), left, right)
            }

            (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                let i64_type = self.context.i64_type();
                let left = self
                    .builder()
                    .build_float_to_unsigned_int(left, i64_type, "ptr_to_int")
                    .unwrap();
                let right = self
                    .builder()
                    .build_float_to_unsigned_int(right, i64_type, "ptr_to_int")
                    .unwrap();
                build_closure(self.builder(), left, right)
            }

            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                build_closure(self.builder(), left, right)
            }

            _ => panic!(),
        }
        .unwrap();

        BasicValueEnum::from(value)
    }

    #[inline(always)]
    pub fn compile_logical_and(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_logical_binop(left, right, |builder, left, right| {
            builder.build_and(left, right, "logical_and")
        })
    }

    #[inline(always)]
    pub fn compile_logical_or(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_logical_binop(left, right, |builder, left, right| {
            builder.build_or(left, right, "logical_or")
        })
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
    pub fn compile_bitwise_and(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_and(left, right, "bitwise_and")
        })
    }

    #[inline(always)]
    pub fn compile_bitwise_or(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_or(left, right, "bitwise_or")
        })
    }

    #[inline(always)]
    pub fn compile_bitwise_xor(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_xor(left, right, "bitwise_xor")
        })
    }

    #[inline(always)]
    pub fn compile_left_shift(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_left_shift(left, right, "left_shift")
        })
    }

    #[inline(always)]
    pub fn compile_right_shift(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        self.compile_int_binop(left, right, |builder, left, right| {
            builder.build_right_shift(left, right, false, "right_shift")
        })
    }
}
