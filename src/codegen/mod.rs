use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue};

use crate::data::mlir::{MidLevelIR, MlirFunction, MlirType, MlirTypeDecl, MlirTypeKind};
use crate::data::symbols::BUILTINS;
use crate::util::str_intern;
use crate::util::str_intern::InternedStr;

// pub fn compile(mlir: &MidLevelIR, name: &str) {
//     let context = Context::create();
//     let builder = context.create_builder();
//     let module = context.create_module(name);
//     let compiler = Compiler::new(mlir, &context, &builder, &module);
// }

struct Compiler<'a, 'mlir, 'ctx> {
    pub(in crate::codegen) mlir: &'mlir MidLevelIR,
    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) builder: &'a Builder<'ctx>,
    pub(in crate::codegen) module: &'a Module<'ctx>,
    pub(in crate::codegen) functions: RefCell<HashMap<InternedStr, FunctionValue<'ctx>>>,
    pub(in crate::codegen) variables: RefCell<HashMap<InternedStr, PointerValue<'ctx>>>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    #[inline(always)]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn compile_builtins(&self) {
        for (ident, builtin) in BUILTINS.iter() {
            let ret_type = self.convert_type(&builtin.return_ty);
            let param_types = builtin
                .params
                .iter()
                .map(|param_type| self.convert_type(param_type).into())
                .collect::<Vec<BasicMetadataTypeEnum>>();
            let fn_type = ret_type.fn_type(&param_types, builtin.varargs);
            let fn_val = self
                .module
                .add_function(ident, fn_type, Some(Linkage::External));
            self.functions
                .borrow_mut()
                .insert(str_intern::intern(ident), fn_val);
        }
    }

    fn compile_function_signature(&self, function: &MlirFunction) -> FunctionValue<'ctx> {
        let ret_type = self.convert_type(&function.ty);
        let param_types = function.parameters.iter().map(|variable_param| {
            let param_type = &variable_param.ty;
            self.convert_type(param_type).into()
        }).collect::<Vec<BasicMetadataTypeEnum>>();
        let fn_type = ret_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(function.ident.as_ref(), fn_type, Some(Linkage::Internal));
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(function.parameters[i].ident.as_ref());
        }
        self.functions.borrow_mut().insert(function.ident.clone(), fn_val);
        fn_val
    }

    fn compile_function(&mut self, function: &MlirFunction) {
        let context_function = self.compile_function_signature(function);
        let entry = self.context.append_basic_block(context_function, "entry");
        self.builder.position_at_end(entry);
        self.fn_value_opt = Some(context_function);
        for (i, arg) in context_function.get_param_iter().enumerate() {
            let arg_name = function.ident.to_string();
            let allocation = self.create_entry_block_allocation(&arg_name);
        }
    }

    fn create_entry_block_allocation(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    fn convert_type(&self, ty: &MlirType) -> BasicTypeEnum<'ctx> {
        // // AnyTypeEnum is used and converted to BasicTypeEnum
        // // so that Void can be preserved to be turned into a pointer.
        let any_type_enum: AnyTypeEnum<'ctx> = match &ty.kind {
            MlirTypeKind::Void => self.context.void_type().into(),
            MlirTypeKind::Char(_) => self.context.i8_type().into(),
            MlirTypeKind::Int(_) => self.context.i32_type().into(),
            MlirTypeKind::Long(_) => self.context.i64_type().into(),
            MlirTypeKind::Float => self.context.f32_type().into(),
            MlirTypeKind::Double => self.context.f64_type().into(),
            MlirTypeKind::Struct(ident) => self.get_struct_type(ident).into(),
        };
        match &ty.decl {
            MlirTypeDecl::Basic => any_type_enum.try_into().expect("Could not convert AnyTypeEnum variant into BasicTypeEnum"),
            MlirTypeDecl::Pointer | MlirTypeDecl::Array(_) => {
                any_type_enum.into_pointer_type().into()
            }
        }
    }


    fn get_struct_type(&self, ident: &InternedStr) -> StructType<'ctx> {
        let _struct = self.mlir.structs.get(ident).expect("struct should exist.");
        let member_types = _struct
            .fields
            .iter()
            .map(|field| self.convert_type(&field.ty))
            .collect::<Vec<_>>();
        self.context.struct_type(&member_types, false)
    }
}
