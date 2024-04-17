use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, StructType};
use inkwell::values::{FunctionValue, PointerValue};

use crate::data::mlir::{MidLevelIR, MlirFunction, MlirType, MlirTypeDecl, MlirTypeKind};
use crate::util::str_intern::InternedStr;

pub struct Compiler<'a, 'ctx> {
    pub(in crate::codegen) mlir: MidLevelIR,

    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) builder: &'a Builder<'ctx>,
    pub(in crate::codegen) module: &'a Module<'ctx>,
    pub(in crate::codegen) function: Option<&'a MlirFunction>,

    pub(in crate::codegen) variables: HashMap<String, PointerValue<'ctx>>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    fn compile_builtins(&self) -> Result<FunctionValue<'ctx>, &'static str> {
        // let ret_type = self.context.f64_type();
        // let args_types = std::iter::repeat(ret_type)
        //     .take(proto.args.len())
        //     .map(|f| f.into())
        //     .collect::<Vec<BasicMetadataTypeEnum>>();
        // let args_types = args_types.as_slice();
        //
        // let fn_type = self.context.f64_type().fn_type(args_types, false);
        // let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);
        //
        // // set arguments names
        // for (i, arg) in fn_val.get_param_iter().enumerate() {
        //     arg.into_float_value().set_name(proto.args[i].as_str());
        // }
        //
        // // finally return built prototype
        // Ok(fn_val)
        todo!()
    }

    fn convert_type(&self, ty: &MlirType) -> BasicTypeEnum {
        let any_type_enum = match &ty.kind {
            MlirTypeKind::Void => AnyTypeEnum::VoidType(self.context.void_type()),
            MlirTypeKind::Char(_) => AnyTypeEnum::IntType(self.context.i8_type()),
            MlirTypeKind::Int(_) => AnyTypeEnum::IntType(self.context.i32_type()),
            MlirTypeKind::Long(_) => AnyTypeEnum::IntType(self.context.i64_type()),
            MlirTypeKind::Float => AnyTypeEnum::FloatType(self.context.f32_type()),
            MlirTypeKind::Double => AnyTypeEnum::FloatType(self.context.f64_type()),
            MlirTypeKind::Struct(ident) => AnyTypeEnum::StructType(self.get_struct_type(ident)),
        };
        let full_type = match &ty.decl {
            MlirTypeDecl::Basic => any_type_enum,
            MlirTypeDecl::Pointer | MlirTypeDecl::Array(_) => {
                AnyTypeEnum::PointerType(any_type_enum.into_pointer_type())
            }
        };
        BasicTypeEnum::try_from(full_type).expect("Type should be able to be converted to basic.")
    }

    fn get_struct_type(&self, ident: &InternedStr) -> StructType {
        let _struct = self.mlir.structs.get(ident).expect("struct should exist.");
        let member_types = _struct
            .fields
            .iter()
            .map(|field| self.convert_type(&field.ty))
            .collect::<Vec<_>>();
        self.context.struct_type(&member_types, false)
    }
}
