use inkwell::{FloatPredicate, IntPredicate};
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirExprKind};
use crate::util::str_intern::InternedStr;

enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}
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

macro_rules! build_bitwise_binop {
    ($compiler:ident, $left:ident, $right:ident, $build_method:ident) => {{
        let left = $compiler.compile_expression($left).into_int_value();
        let right = $compiler.compile_expression($right).into_int_value();

        let value = $compiler
            .builder()
            .$build_method(left, right, stringify!($build_method))
            .unwrap();

        BasicValueEnum::from(value)
    }};
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

    pub(super) fn compile_assignment(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        debug_assert!(left.is_lval);
        let assign_value = self.compile_expression(right);

        let assign_ptr = match &*left.kind {
            MlirExprKind::Variable(id) => self.get_pointer(id),
            MlirExprKind::Deref(expr) => self.compile_expression(expr).into_pointer_value(),
            MlirExprKind::Member(_struct, member) => self.get_struct_member_pointer(
                self.convert_type(&left.ty.as_basic()),
                _struct,
                member,
            ),
            MlirExprKind::Index(array, index) => {
                self.get_array_index_pointer(self.convert_type(&left.ty.as_basic()), array, index)
            }
            _ => self.compile_expression(left).into_pointer_value(),
        };

        self.builder().build_store(assign_ptr, assign_value);

        assign_value
    }

    fn get_struct_member_pointer(
        &mut self,
        access_ty: BasicTypeEnum<'ctx>,
        _struct: &MlirExpr,
        member: &InternedStr,
    ) -> PointerValue<'ctx> {
        let struct_ident = _struct.ty.kind.get_struct_ident();
        let _struct = self.compile_expression(_struct).into_pointer_value();
        let member_offset = self.mlir.get_struct_member_offset(&struct_ident, member);
        self.builder()
            .build_struct_gep(access_ty, _struct, member_offset, "get_struct_member_ptr")
            .unwrap()
    }

    fn get_array_index_pointer(
        &mut self,
        access_ty: BasicTypeEnum<'ctx>,
        array: &MlirExpr,
        index: &MlirExpr,
    ) -> PointerValue<'ctx> {
        let array_ptr = self.compile_expression(array).into_pointer_value();
        let index_value = self.compile_expression(index).into_int_value();
        unsafe {
            self.builder()
                .build_gep(access_ty, array_ptr, &[index_value], "get_array_index_ptr")
                .unwrap()
        }
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

    pub(super) fn compile_logical_and(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        let (logical_left, logical_right) = self.compile_binary_logical_expr(left, right);

        let value = self
            .builder()
            .build_and(logical_left, logical_right, "logical_and")
            .unwrap();

        BasicValueEnum::from(value)
    }

    pub(super) fn compile_logical_or(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        let (logical_left, logical_right) = self.compile_binary_logical_expr(left, right);

        let value = self
            .builder()
            .build_or(logical_left, logical_right, "logical_or")
            .unwrap();

        BasicValueEnum::from(value)
    }

    pub(super) fn compile_bitwise_and(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        build_bitwise_binop!(self, left, right, build_and)
    }

    pub(super) fn compile_bitwise_or(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        build_bitwise_binop!(self, left, right, build_or)
    }
}
