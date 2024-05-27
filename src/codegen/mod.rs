use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::fmt::Formatter;
use std::rc::Rc;

use derive_new::new;
use inkwell::{FloatPredicate, IntPredicate};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntMathValue, PointerValue,
};
use inkwell::values::IntValue;
use log::debug;
use serde::ser::SerializeTuple;

use crate::data::mlir::{
    MlirBlock, MlirExpr, MlirExprKind, MlirFunction, MlirLiteral, MlirModule, MlirStmt, MlirStruct,
    MlirType, MlirTypeDecl, MlirTypeKind, MlirVariable, VOID_TYPE,
};
use crate::data::symbols::BUILTINS;
use crate::util::str_intern;
use crate::util::str_intern::InternedStr;

mod declarations;
mod expressions;
mod statements;

// pub fn compile(mlir: &MidLevelIR, name: &str) {
//     let context = Context::create();
//     let builder = context.create_builder();
//     let module = context.create_module(name);
//     let compiler = Compiler::new(mlir, &context, &builder, &module);
// }

pub struct Compiler<'a, 'mlir, 'ctx> {
    pub(in crate::codegen) mlir: &'mlir MlirModule,
    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) builder: Option<Builder<'ctx>>,
    pub(in crate::codegen) module: &'a Module<'ctx>,
    pub(in crate::codegen) functions: HashMap<InternedStr, FunctionValue<'ctx>>,
    pub(in crate::codegen) variables: HashMap<usize, PointerValue<'ctx>>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
    pub(in crate::codegen) ptr_types: HashMap<PointerValue<'ctx>, BasicTypeEnum<'ctx>>,
    pub(in crate::codegen) struct_types: HashMap<InternedStr, StructType<'ctx>>,
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    #[inline(always)]
    pub(in crate::codegen) fn insert_pointer(&mut self, uid: usize, ptr: PointerValue<'ctx>) {
        self.variables.insert(uid, ptr);
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_pointer(&self, uid: &usize) -> PointerValue<'ctx> {
        *self.variables.get(uid).unwrap()
    }

    #[inline(always)]
    pub(in crate::codegen) fn add_pointer_type(
        &mut self,
        ptr: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) {
        self.ptr_types.insert(ptr, ty);
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_pointer_type(
        &self,
        ptr: &PointerValue<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        *self.ptr_types.get(ptr).unwrap()
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
    pub(in crate::codegen) fn get_struct_member_index(
        &self,
        _struct_name: &InternedStr,
        member: &InternedStr,
    ) -> u64 {
        *self
            .struct_member_indices
            .get(_struct_name)
            .expect("Struct does not exist in map!")
            .get(member)
            .expect("Struct member does not exist in map!")
    }

    #[inline(always)]
    pub(in crate::codegen) fn get_struct_type(&self, ident: &InternedStr) -> StructType<'ctx> {
        *self.struct_types.get(ident).unwrap()
    }

    fn compile_builtins(&mut self) {
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

            self.functions.insert(str_intern::intern(ident), fn_val);
        }
    }

    fn compile_function_signature(&mut self, function: &MlirFunction) -> FunctionValue<'ctx> {
        let param_types: Vec<BasicMetadataTypeEnum> = function
            .parameters
            .iter()
            .map(|variable_param| {
                let param_type = &variable_param.ty;
                self.convert_type(param_type).into()
            })
            .collect();

        // void types 'fn_type' method is only accessible via VoidType directly
        let fn_type = if *function.ty == VOID_TYPE {
            self.context.void_type().fn_type(&param_types, false)
        } else {
            self.convert_type(&function.ty).fn_type(&param_types, false)
        };

        let fn_val =
            self.module
                .add_function(function.ident.as_ref(), fn_type, Some(Linkage::Internal));

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(function.parameters[i].ident.as_ref());
        }

        self.functions.insert(function.ident.clone(), fn_val);

        fn_val
    }

    fn compile_function(&mut self, function: &MlirFunction) {
        let context_function = self.compile_function_signature(function);
        let entry = self.context.append_basic_block(context_function, "entry");

        self.builder().position_at_end(entry);
        self.fn_value_opt = Some(context_function);
        self.variables.reserve(function.parameters.len());

        for (llvm_param, mlir_param) in context_function
            .get_param_iter()
            .zip(function.parameters.iter())
        {
            let allocation =
                self.create_entry_block_allocation(llvm_param.get_type(), &mlir_param.ident);

            self.builder().build_store(allocation, llvm_param).unwrap();

            self.variables.insert(mlir_param.uid, allocation);
        }

        todo!("build body and compile statements");
    }

    fn process_function_body(&mut self, mlir_block: &MlirBlock) {
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

        while let (Some(mlir_bb), Some(llvm_bb)) = (mlir_block_iter.next(), llvm_block_iter.next())
        {
            let next_llvm_bb = llvm_block_iter.peek().copied();
            self.builder().position_at_end(llvm_bb);
            self.compile_mlir_basic_block(mlir_bb);
        }
    }

    pub(in crate::codegen) fn deref_pointer_value(
        &mut self,
        ptr: PointerValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ptr_type = self.get_pointer_type(&ptr);
        self.builder()
            .build_load(ptr_type, ptr, "ptr_deref_val")
            .unwrap()
    }

    pub(in crate::codegen) fn create_entry_block_allocation(
        &self,
        ty: BasicTypeEnum<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = self.fn_value().get_first_basic_block().unwrap();
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }
        builder.build_alloca(ty, name).unwrap()
    }

    pub(in crate::codegen) fn convert_type(&self, ty: &MlirType) -> BasicTypeEnum<'ctx> {
        let any_type_enum = self.get_type_kind_as_llvm_any_type(&ty.kind);
        match &ty.decl {
            MlirTypeDecl::Array(size) => any_type_enum.into_array_type().into(),
            MlirTypeDecl::Pointer => any_type_enum.into_pointer_type().into(),
            MlirTypeDecl::Basic => any_type_enum
                .try_into()
                .expect("Could not convert AnyTypeEnum variant into BasicTypeEnum"),
        }
    }

    fn get_type_kind_as_llvm_any_type(&self, kind: &MlirTypeKind) -> AnyTypeEnum<'ctx> {
        match kind {
            MlirTypeKind::Void => self.context.void_type().into(),
            MlirTypeKind::Char(_) => self.context.i8_type().into(),
            MlirTypeKind::Int(_) => self.context.i32_type().into(),
            MlirTypeKind::Long(_) => self.context.i64_type().into(),
            MlirTypeKind::Float => self.context.f32_type().into(),
            MlirTypeKind::Double => self.context.f64_type().into(),
            MlirTypeKind::Struct(ident) => self.get_struct_type(ident).into(),
        }
    }

    fn create_struct_type(&self, _struct: &'mlir MlirStruct) -> StructType<'ctx> {
        let member_types = _struct
            .members
            .iter()
            .map(|field| self.convert_type(&field.ty))
            .collect::<Vec<_>>();
        self.context.struct_type(&member_types, false)
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
