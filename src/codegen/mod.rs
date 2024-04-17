use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, PointerValue};

use crate::data::mlir::{MidLevelIR, MlirFunction};

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
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }
}
