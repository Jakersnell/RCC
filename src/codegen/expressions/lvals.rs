use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirExprKind};
use crate::util::str_intern::InternedStr;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub(super) fn compile_deref(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let ptr = self.compile_expression(expr).into_pointer_value();
        let ptr_type = self.convert_type(&expr.ty.as_basic());
        self.builder()
            .build_load(ptr_type, ptr, "ptr_deref_val")
            .unwrap()
    }

    pub(super) fn compile_address_of(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let ptr = match &*expr.kind {
            MlirExprKind::Variable(ident) => self.get_pointer(ident),
            MlirExprKind::Deref(pointer) => self.compile_expression(pointer).into_pointer_value(),
            MlirExprKind::Assign(left, right) => self
                .compile_assignment(left, right, true)
                .into_pointer_value(),
            MlirExprKind::Index(array, index) => {
                self.get_array_index_pointer(self.convert_type(&array.ty.as_basic()), array, index)
            }
            MlirExprKind::Member(_struct, member) => {
                self.get_struct_member_pointer(self.convert_type(&expr.ty), _struct, member)
            }
            _ => panic!(),
        };
        BasicValueEnum::from(ptr)
    }

    pub(super) fn compile_assignment(
        &mut self,
        left: &MlirExpr,
        right: &MlirExpr,
        return_ptr: bool,
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

        self.builder()
            .build_store(assign_ptr, assign_value)
            .unwrap();

        if return_ptr {
            BasicValueEnum::from(assign_ptr)
        } else {
            assign_value
        }
    }

    pub(super) fn get_struct_member_pointer(
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

    pub(super) fn get_array_index_pointer(
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
}
