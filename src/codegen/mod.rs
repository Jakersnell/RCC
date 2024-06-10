use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::fmt::{format, Formatter};
use std::rc::Rc;

use derive_new::new;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
};
use inkwell::values::{
    BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntMathValue, PointerValue,
};
use inkwell::values::IntValue;
use log::debug;
use serde::ser::SerializeTuple;

use crate::data::ast::BinaryOp::Add;
use crate::data::mlir::{
    MlirBlock, MlirExpr, MlirExprKind, MlirFunction, MlirLiteral, MlirModule, MlirStmt, MlirStruct,
    MlirType, MlirTypeDecl, MlirTypeKind, MlirVariable, MlirVarInit, VOID_PTR, VOID_TYPE,
};
use crate::data::symbols::BUILTINS;
use crate::util::{Locatable, str_intern};
use crate::util::str_intern::InternedStr;

pub(in crate::codegen) mod binary_expressions;
pub(in crate::codegen) mod declarations;
pub(in crate::codegen) mod expressions;
pub(in crate::codegen) mod lvals;
pub(in crate::codegen) mod statements;

pub struct Compiler<'a, 'mlir, 'ctx> {
    pub(in crate::codegen) mlir: &'mlir MlirModule,
    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) builder: Option<Builder<'ctx>>,
    pub(in crate::codegen) module: &'a Module<'ctx>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
    pub(in crate::codegen) struct_types: HashMap<InternedStr, StructType<'ctx>>,
    pub(in crate::codegen) block_has_jumped: bool,
    pub(in crate::codegen) init_in_main:
        Vec<(InternedStr, BasicTypeEnum<'ctx>, Option<&'mlir MlirVarInit>)>,
    functions: HashMap<InternedStr, FunctionValue<'ctx>>,
    variables: HashMap<InternedStr, PointerValue<'ctx>>,
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn new(mlir: &'mlir MlirModule, context: &'ctx Context, module: &'a Module<'ctx>) -> Self {
        let mut compiler = Self {
            mlir,
            context,
            module,
            functions: Default::default(),
            variables: Default::default(),
            struct_types: Default::default(),
            fn_value_opt: None,
            builder: None,
            block_has_jumped: false,
            init_in_main: vec![],
        };
        compiler.builder = Some(compiler.context.create_builder());
        compiler.compile_builtins();
        compiler
    }

    pub fn compile(mut self) -> Result<LLVMString, LLVMString> {
        for _struct in self.mlir.structs.iter() {
            self.create_struct_type(_struct);
        }

        for global in self.mlir.globals.iter() {
            self.compile_global(global);
        }

        for function in self.mlir.functions.values() {
            self.compile_function(function);
        }

        self.module.verify()?;
        let llir = self.module.print_to_string();
        Ok(llir)
    }

    #[inline(always)]
    pub(in crate::codegen) fn insert_pointer(&mut self, uid: InternedStr, ptr: PointerValue<'ctx>) {
        self.variables.insert(uid, ptr);
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_pointer(&self, ident: &InternedStr) -> PointerValue<'ctx> {
        *self.variables.get(ident).unwrap()
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_next_block(&self) -> Option<BasicBlock<'ctx>> {
        self.builder().get_insert_block()?.get_next_basic_block()
    }

    #[inline(always)]
    pub(in crate::codegen) fn builder(&self) -> &Builder<'ctx> {
        self.builder.as_ref().unwrap()
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_block_by_name(&self, name: &str) -> Option<BasicBlock<'ctx>> {
        self.fn_value()
            .get_basic_block_iter()
            .find(|block| block.get_name().to_str().unwrap() == name)
    }

    #[inline(always)]
    pub(in crate::codegen) fn last_block(&self) -> BasicBlock<'ctx> {
        self.fn_value().get_last_basic_block().unwrap()
    }

    #[inline(always)]
    pub(in crate::codegen) fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_struct_type(&self, ident: &InternedStr) -> StructType<'ctx> {
        *self
            .struct_types
            .get(ident)
            .unwrap_or_else(|| panic!("Struct '{}' does not exist!", ident))
    }

    #[inline(always)]
    fn compile_global(&mut self, global: &'mlir MlirVariable) {
        self.compile_variable_declaration(global, true);
    }

    fn compile_builtins(&mut self) {
        for (ident, builtin) in BUILTINS.iter() {
            let param_types = builtin
                .params
                .iter()
                .map(|param_type| self.convert_type(param_type).into())
                .collect::<Vec<BasicMetadataTypeEnum>>();

            let fn_type =
                self.convert_function_type(&builtin.return_ty, &param_types, builtin.varargs);

            let fn_val = self
                .module
                .add_function(ident, fn_type, Some(Linkage::External));

            self.functions.insert(str_intern::intern(ident), fn_val);
        }
    }

    fn compile_function_signature(&mut self, function: &MlirFunction) -> FunctionValue<'ctx> {
        let param_types: Vec<BasicMetadataTypeEnum> = function
            .parameters
            .iter()
            .map(|variable_param| {
                let param_type = &variable_param.ty;
                let ty_enum = self.convert_type(param_type).into();
                ty_enum
            })
            .collect();

        // void types 'fn_type' method is only accessible via VoidType directly
        let fn_type = self.convert_function_type(&function.ty, &param_types, false);

        let fn_val = self
            .module
            .add_function(function.ident.as_ref(), fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(function.parameters[i].ident.as_ref());
        }

        self.functions.insert(function.ident.clone(), fn_val);

        fn_val
    }

    fn init_static_globals(&mut self) {
        let init_in_main = std::mem::take(&mut self.init_in_main);
        for (ident, ty, initializer) in init_in_main {
            let ptr = self.get_pointer(&ident);
            self.initialize_variable(ty, ptr, initializer);
        }
    }

    fn compile_function(&mut self, function: &'mlir MlirFunction) {
        let context_function = self.compile_function_signature(function);
        let entry = self.context.append_basic_block(context_function, "entry");

        self.builder().position_at_end(entry);
        self.fn_value_opt = Some(context_function);
        self.variables.reserve(function.parameters.len());

        if function.ident.as_ref() == "main" {
            self.init_static_globals();
        }

        for (llvm_param, mlir_param) in context_function
            .get_param_iter()
            .zip(function.parameters.iter())
        {
            let MlirVariable {
                span,
                ty: mlir_type,
                ident,
                is_const,
                initializer,
            } = &mlir_param.value;

            let ty = if matches!(&mlir_type.decl, MlirTypeDecl::Array(_)) {
                mlir_type.as_basic()
            } else {
                mlir_type.value.clone()
            };
            let ty = self.convert_type(&ty);

            let allocation = match &mlir_param.ty.decl {
                MlirTypeDecl::Array(size) => self.create_entry_block_array_allocation(ty, *size),
                MlirTypeDecl::Pointer | MlirTypeDecl::Basic => {
                    self.create_entry_block_allocation(ty, ident)
                }
            };

            self.builder().build_store(allocation, llvm_param).unwrap();

            self.insert_pointer(mlir_param.ident.clone(), allocation)
        }

        self.process_function_body(&function.body);

        if cfg!(debug_assertions) && !self.fn_value().verify(true) {
            self.fn_value().print_to_stderr();
            panic!();
        }

        self.fn_value_opt = None;
    }

    fn process_function_body(&mut self, mlir_block: &'mlir MlirBlock) {
        let mlir_basic_blocks = pre_construct_blocks(mlir_block);
        let function = self.fn_value();

        for (block_number, block) in mlir_basic_blocks.iter().enumerate() {
            let name = if let Some(label) = &block.label {
                label.to_string()
            } else {
                format!("basic_block_{}", block_number)
            };
            self.context.append_basic_block(function, &name);
        }

        if cfg!(debug_assertions) {
            let block_count = function.get_basic_block_iter().count();
            assert_eq!(mlir_basic_blocks.len() + 1, block_count); // + 1 to account for entry block
        }

        let mut llvm_block_iter = function.get_basic_block_iter().peekable();
        llvm_block_iter.next(); // skip entry block

        let mut mlir_block_iter = mlir_basic_blocks.iter().peekable();
        self.block_has_jumped = false;
        while let (Some(mlir_bb), Some(llvm_bb)) = (mlir_block_iter.next(), llvm_block_iter.next())
        {
            if !self.block_has_jumped {
                self.builder().build_unconditional_branch(llvm_bb);
            }
            self.builder().position_at_end(llvm_bb);
            self.block_has_jumped = false;
            self.compile_mlir_basic_block(mlir_bb);
        }
    }

    fn create_entry_builder(&self) -> Builder<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        match entry.get_last_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        builder
    }

    pub(in crate::codegen) fn create_entry_block_allocation(
        &self,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.create_entry_builder();
        builder.build_alloca(ty, name).unwrap()
    }

    pub(in crate::codegen) fn create_entry_block_array_allocation(
        &mut self,
        element_ty: BasicTypeEnum<'ctx>,
        size: u64,
    ) -> PointerValue<'ctx> {
        let builder = self.create_entry_builder();
        let size = self.context.i32_type().const_int(size, false);
        builder
            .build_array_alloca(element_ty, size, "alloc_array")
            .unwrap()
    }

    pub(in crate::codegen) fn convert_function_type(
        &self,
        func_ty: &MlirType,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        if func_ty.decl == MlirTypeDecl::Basic && func_ty.kind == MlirTypeKind::Void {
            self.context.void_type().fn_type(param_types, is_var_args)
        } else {
            self.convert_type(func_ty).fn_type(param_types, is_var_args)
        }
    }

    pub(in crate::codegen) fn convert_type(&self, ty: &MlirType) -> BasicTypeEnum<'ctx> {
        if ty == &VOID_PTR {
            return self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into();
        }

        let basic_type = self.get_type_kind_as_llvm_any_type(&ty.kind);

        let finished_type = match &ty.decl {
            MlirTypeDecl::Array(size) => basic_type.into_array_type().into(),
            MlirTypeDecl::Pointer => basic_type.ptr_type(AddressSpace::default()).into(),
            MlirTypeDecl::Basic => basic_type,
        };

        finished_type
    }

    fn get_type_kind_as_llvm_any_type(&self, kind: &MlirTypeKind) -> BasicTypeEnum<'ctx> {
        match kind {
            MlirTypeKind::Char(_) => self.context.i8_type().into(),
            MlirTypeKind::Int(_) => self.context.i32_type().into(),
            MlirTypeKind::Long(_) => self.context.i64_type().into(),
            MlirTypeKind::Float => self.context.f32_type().into(),
            MlirTypeKind::Double => self.context.f64_type().into(),
            MlirTypeKind::Struct(ident) => self.get_struct_type(ident).into(),
            _ => panic!(),
        }
    }

    fn create_struct_type(&mut self, _struct: &'mlir MlirStruct) -> StructType<'ctx> {
        let member_types = _struct
            .members
            .iter()
            .map(|field| self.convert_type(&field.ty))
            .collect::<Vec<_>>();
        let struct_type = self.context.struct_type(&member_types, false);
        self.struct_types.insert(_struct.ident.clone(), struct_type);
        struct_type
    }
}

pub fn pre_construct_blocks(function_block: &MlirBlock) -> Vec<MlirBasicBlock<'_>> {
    let mut blocks = Vec::new();
    let mut stmts = Vec::new();
    let mut label = None;

    macro_rules! new_block {
        () => {
            new_block(&mut blocks, &mut stmts, &mut label)
        };
    };

    for stmt in function_block.iter() {
        match stmt {
            MlirStmt::Label(ident) => {
                new_block!();
                label = Some(ident.clone());
            }

            MlirStmt::Goto(_) | MlirStmt::CondGoto(_, _, _) | MlirStmt::Return(_) => {
                stmts.push(stmt);
                new_block!();
            }

            MlirStmt::Expression(_) | MlirStmt::VariableDeclaration(_) => {
                stmts.push(stmt);
            }

            MlirStmt::Block(_) => panic!("blocks mut be flattened at this stage in the MLIR"),
        }
    }

    blocks
}

fn new_block<'mlir>(
    blocks: &mut Vec<MlirBasicBlock<'mlir>>,
    stmts: &mut Vec<&'mlir MlirStmt>,
    label: &mut Option<InternedStr>,
) {
    let label_is_none = label.is_none();
    let _label = label.take();
    let basic_block = MlirBasicBlock::new(_label, std::mem::take(stmts));
    if basic_block.label.is_some() || !basic_block.stmts.is_empty() {
        blocks.push(basic_block);
    }
}

#[derive(Debug, new)]
pub struct MlirBasicBlock<'mlir> {
    pub label: Option<InternedStr>,
    pub stmts: Vec<&'mlir MlirStmt>,
}

impl std::fmt::Display for MlirBasicBlock<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\n--- {:?} ---", self.label);
        for stmt in self.stmts.iter() {
            writeln!(f, "{}", stmt)?;
        }
        writeln!(f, "---")
    }
}
