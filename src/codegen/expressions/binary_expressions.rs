use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirExprKind};
use crate::util::str_intern::InternedStr;

macro_rules! build_arithmetic_binop {
    (
        $compiler:ident, $left:ident, $right:ident;
        $int_case:ident;
        $float_case:ident;
    ) => {
        match $compiler.compile_binary_expr($left, $right) {
            (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                BasicValueEnum::from(
                    $compiler
                        .builder()
                        .$int_case(left, right, stringify!($int_case))
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
    ) -> BasicValueEnum<'ctx> {
        build_arithmetic_binop!(
            self, left, right;
            build_int_signed_div;
            build_float_div;
        )
    }
}
