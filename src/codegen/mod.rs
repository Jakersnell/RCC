use std::collections::HashMap;

use derive_new::new;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};

use crate::data::mlir::{MidLevelIR, MlirBlock, MlirExpr, MlirExprKind, MlirFunction, MlirStmt, MlirType, MlirTypeDecl, MlirTypeKind, VOID_TYPE};
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
    pub(in crate::codegen) functions: HashMap<InternedStr, FunctionValue<'ctx>>,
    pub(in crate::codegen) variables: HashMap<InternedStr, PointerValue<'ctx>>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    #[inline(always)]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
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

        self.builder.position_at_end(entry);
        self.fn_value_opt = Some(context_function);
        self.variables.reserve(function.parameters.len());

        for (i, arg) in context_function.get_param_iter().enumerate() {
            let arg_name = function.parameters[i].ident.clone();
            let allocation = self.create_entry_block_allocation(arg.get_type(), &arg_name);

            self.builder.build_store(allocation, arg).unwrap();

            self.variables.insert(arg_name, allocation);
        }

        todo!("build body and compile statements");
    }

    fn compile_statement(&mut self, statement: &MlirStmt) {
        match statement {
            MlirStmt::Expression(expression) => todo!(),
            MlirStmt::VariableDeclaration(var) => todo!(),
            MlirStmt::Label(_) => todo!(),
            MlirStmt::Goto(_) => todo!(),
            MlirStmt::GotoFalse(_, _) => todo!(),
            MlirStmt::Return(expression) => self.compile_return_statement(expression),
            MlirStmt::Block(_) => unreachable!(
                "All blocks must be flattened. No blocks should exist in the MLIR at this stage"
            ),
        }
    }

    fn compile_return_statement(&mut self, expression_opt: &Option<MlirExpr>) {
        let expression = expression_opt
            .as_ref()
            .map(|expression| self.compile_expression(expression));
        let trait_ref_expression: Option<&dyn BasicValue<'ctx>> =
            if let Some(expression) = expression.as_ref() {
                Some(expression)
            } else {
                None
            };
        let _return = self.builder.build_return(trait_ref_expression).unwrap();
    }

    fn compile_expression(&mut self, expression: &MlirExpr) -> BasicValueEnum<'ctx> {
        match &*expression.kind {
            MlirExprKind::Literal(_) => todo!(),
            MlirExprKind::Variable(_) => todo!(),
            MlirExprKind::PostIncrement(_) => todo!(),
            MlirExprKind::PostDecrement(_) => todo!(),
            MlirExprKind::Negate(_) => todo!(),
            MlirExprKind::LogicalNot(_) => todo!(),
            MlirExprKind::BitwiseNot(_) => todo!(),
            MlirExprKind::Deref(_) => todo!(),
            MlirExprKind::AddressOf(_) => todo!(),
            MlirExprKind::Add(_, _) => todo!(),
            MlirExprKind::Sub(_, _) => todo!(),
            MlirExprKind::Mul(_, _) => todo!(),
            MlirExprKind::Div(_, _) => todo!(),
            MlirExprKind::Mod(_, _) => todo!(),
            MlirExprKind::Equal(_, _) => todo!(),
            MlirExprKind::NotEqual(_, _) => todo!(),
            MlirExprKind::GreaterThan(_, _) => todo!(),
            MlirExprKind::GreaterThanEqual(_, _) => todo!(),
            MlirExprKind::LessThan(_, _) => todo!(),
            MlirExprKind::LessThanEqual(_, _) => todo!(),
            MlirExprKind::LogicalAnd(_, _) => todo!(),
            MlirExprKind::LogicalOr(_, _) => todo!(),
            MlirExprKind::BitwiseAnd(_, _) => todo!(),
            MlirExprKind::BitwiseOr(_, _) => todo!(),
            MlirExprKind::BitwiseXor(_, _) => todo!(),
            MlirExprKind::LeftShift(_, _) => todo!(),
            MlirExprKind::RightShift(_, _) => todo!(),
            MlirExprKind::Assign(_, _) => todo!(),
            MlirExprKind::FunctionCall { .. } => todo!(),
            MlirExprKind::Index(_, _) => todo!(),
            MlirExprKind::Member(_, _) => todo!(),
            MlirExprKind::Cast(_, _) => todo!(),
        }
    }

    fn create_entry_block_allocation(
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

    fn convert_type(&self, ty: &MlirType) -> BasicTypeEnum<'ctx> {
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

pub fn pre_construct_blocks(function_block: &MlirBlock) -> Vec<MlirBasicBlock<'_>> {
    let mut blocks = Vec::new();
    let mut stmts = Vec::new();
    let mut label = None;

    macro_rules! new_block {
        () => { new_block(&mut blocks, &mut stmts, &mut label) }
    };

    for stmt in function_block.iter() {
        match stmt {
            MlirStmt::Label(ident) => {
                new_block!();
                label = Some(ident.clone());
            }

            MlirStmt::Goto(_) |
            MlirStmt::GotoFalse(_, _) |
            MlirStmt::Return(_) => {
                stmts.push(stmt);
                new_block!();
            }

            _ => {
                stmts.push(stmt);
            }

            MlirStmt::Block(_) => panic!("blocks mut be flattened at this stage in the MLIR"),
        }
    }

    blocks
}

fn new_block<'mlir>(blocks: &mut Vec<MlirBasicBlock<'mlir>>, stmts: &mut Vec<&'mlir MlirStmt>, label: &mut Option<InternedStr>) {
    let label_is_none = label.is_none();
    let _label = label.take();
    let basic_block = MlirBasicBlock::new(_label, std::mem::take(stmts));
    blocks.push(basic_block);
}

#[derive(Debug, new)]
pub struct MlirBasicBlock<'mlir> {
    pub label: Option<InternedStr>,
    pub stmts: Vec<&'mlir MlirStmt>,
}
