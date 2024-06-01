use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

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

        let assign_ptr = self.get_lval_as_pointer(left);

        self.builder()
            .build_store(assign_ptr, assign_value)
            .unwrap();

        if return_ptr {
            BasicValueEnum::from(assign_ptr)
        } else {
            assign_value
        }
    }

    fn get_lval_as_pointer(&mut self, lval: &MlirExpr) -> PointerValue<'ctx> {
        match &*lval.kind {
            MlirExprKind::Variable(id) => self.get_pointer(id),
            MlirExprKind::Deref(expr) => self.compile_expression(expr).into_pointer_value(),
            MlirExprKind::Member(_struct, member) => self.compile_struct_member_pointer(
                self.convert_type(&lval.ty.as_basic()),
                _struct,
                member,
            ),
            MlirExprKind::Index(array, index) => self.compile_array_index_pointer(
                self.convert_type(&lval.ty.as_basic()),
                array,
                index,
            ),
            _ => panic!(),
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
        // get ptr
        // load value
        // add/sub
        // store add/sub value
        // return prior value
        let ptr = self.get_lval_as_pointer(expr);
        let pointee_type = self.convert_type(&expr.ty);
        let loaded_value = self
            .builder()
            .build_load(pointee_type, ptr, "load_ptr")
            .unwrap();
        let store_value = match (loaded_value, pointee_type) {
            (BasicValueEnum::IntValue(int), BasicTypeEnum::IntType(int_type)) => {
                let int_one = int_type.const_int(1, false);
                let value = if inc {
                    self.builder().build_int_add(int, int_one, "inc_int")
                } else {
                    self.builder().build_int_sub(int, int_one, "int_dec")
                }
                .unwrap();
                BasicValueEnum::from(value)
            }
            (BasicValueEnum::FloatValue(float), BasicTypeEnum::FloatType(float_type)) => {
                let float_one = float_type.const_float(1.0);
                let value = if inc {
                    self.builder()
                        .build_float_add(float, float_one, "float_add")
                } else {
                    self.builder()
                        .build_float_sub(float, float_one, "float_sub")
                }
                .unwrap();
                BasicValueEnum::from(value)
            }
            (BasicValueEnum::PointerValue(ptr), BasicTypeEnum::PointerType(ptr_type)) => {
                let ptr_to_int = self
                    .builder()
                    .build_ptr_to_int(ptr, self.context.i64_type(), "ptr_to_int")
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

                let value = self
                    .builder()
                    .build_int_to_ptr(new_int_value, ptr_type, "int_to_ptr")
                    .unwrap();
                BasicValueEnum::from(value)
            }
            _ => panic!(),
        };
        self.builder().build_store(ptr, store_value);
        loaded_value
    }
}
