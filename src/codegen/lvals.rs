use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue};

use crate::codegen::Compiler;
use crate::data::mlir::{MlirExpr, MlirExprKind, MlirType};
use crate::util::str_intern::InternedStr;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn compile_deref(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let ptr = self.compile_expression(expr).into_pointer_value();
        let ptr_type = self.convert_type(&expr.ty.as_basic());
        self.builder()
            .build_load(ptr_type, ptr, "ptr_deref_val")
            .unwrap()
    }

    pub fn compile_address_of(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let ptr = match &*expr.kind {
            MlirExprKind::Variable(ident) => self.get_pointer(ident),
            MlirExprKind::Deref(pointer) => self.compile_expression(pointer).into_pointer_value(),
            MlirExprKind::Assign(left, right) => self
                .compile_assignment(left, right, true)
                .into_pointer_value(),
            MlirExprKind::Index(array, index) => self.compile_array_index_pointer(
                self.convert_type(&array.ty.as_basic()),
                array,
                index,
            ),
            MlirExprKind::Member(_struct, member) => {
                self.compile_struct_member_pointer(self.convert_type(&expr.ty), _struct, member)
            }
            _ => panic!(),
        };
        BasicValueEnum::from(ptr)
    }

    pub fn compile_assignment(
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
            MlirExprKind::Member(_struct, member) => self.compile_struct_member_pointer(
                self.convert_type(&left.ty.as_basic()),
                _struct,
                member,
            ),
            MlirExprKind::Index(array, index) => self.compile_array_index_pointer(
                self.convert_type(&left.ty.as_basic()),
                array,
                index,
            ),
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

    pub fn compile_struct_member_pointer(
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

    pub fn compile_array_index_pointer(
        &mut self,
        access_ty: BasicTypeEnum<'ctx>,
        array: &MlirExpr,
        index: &MlirExpr,
    ) -> PointerValue<'ctx> {
        let array_ptr = self.compile_expression(array).into_pointer_value();
        let index_value = self.compile_expression(index).into_int_value();
        self.get_array_index_pointer(access_ty, array_ptr, index_value)
    }

    pub fn get_array_index_pointer(
        &mut self,
        access_ty: BasicTypeEnum<'ctx>,
        array_ptr: PointerValue<'ctx>,
        index_value: IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        unsafe {
            self.builder()
                .build_gep(access_ty, array_ptr, &[index_value], "get_array_index_ptr")
                .unwrap()
        }
    }

    pub fn compile_array_access(
        &mut self,
        array: &MlirExpr,
        index: &MlirExpr,
        element_type: &MlirType,
    ) -> BasicValueEnum<'ctx> {
        let element_type = self.convert_type(element_type);
        let array_index_ptr = self.compile_array_index_pointer(element_type, array, index);
        self.builder()
            .build_load(element_type, array_index_ptr, "load_element_from_array")
            .unwrap()
    }

    pub fn compile_member_access(
        &mut self,
        _struct: &MlirExpr,
        member: &InternedStr,
        member_type: &MlirType,
    ) -> BasicValueEnum<'ctx> {
        let member_type = self.convert_type(member_type);
        let member_ptr = self.compile_struct_member_pointer(member_type, _struct, member);
        self.builder()
            .build_load(member_type, member_ptr, "load_member_ptr")
            .unwrap()
    }

    pub fn compile_variable_access(&mut self, ty: &MlirType, id: usize) -> BasicValueEnum<'ctx> {
        let pointee_ty = self.convert_type(ty);
        let ptr = *self
            .variables
            .get(&id)
            .expect("Invalid variable accesses must be handled in Analysis.");
        self.builder()
            .build_load(pointee_ty, ptr, "access_variable")
            .unwrap()
    }

    pub fn compile_post_increment(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_post_inc_or_dec(expr, true)
    }

    pub fn compile_post_decrement(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_post_inc_or_dec(expr, false)
    }

    fn compile_post_inc_or_dec(&mut self, expr: &MlirExpr, inc: bool) -> BasicValueEnum<'ctx> {
        debug_assert!(expr.is_lval);
        let ptr = self.compile_expression(expr).into_pointer_value();

        let ir_ty = self.convert_type(&expr.ty);
        let ptr_value = self
            .builder()
            .build_load(ir_ty, ptr, "post_inc_load_value")
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
}
