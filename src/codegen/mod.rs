use std::collections::HashMap;
use std::fmt::Formatter;

use derive_new::new;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntMathValue, PointerValue};
use inkwell::values::IntValue;

use crate::data::mlir::{
    MidLevelIR, MlirBlock, MlirExpr, MlirExprKind, MlirFunction, MlirLiteral, MlirStmt, MlirType,
    MlirTypeDecl, MlirTypeKind, MlirVariable, VOID_TYPE,
};
use crate::data::symbols::BUILTINS;
use crate::util::str_intern;
use crate::util::str_intern::InternedStr;

// pub fn compile(mlir: &MidLevelIR, name: &str) {
//     let context = Context::create();
//     let builder = context.create_builder();
//     let module = context.create_module(name);
//     let compiler = Compiler::new(mlir, &context, &builder, &module);
// }

pub struct Compiler<'a, 'mlir, 'ctx> {
    pub(in crate::codegen) mlir: &'mlir MidLevelIR,
    pub(in crate::codegen) context: &'ctx Context,
    pub(in crate::codegen) builder: Option<Builder<'ctx>>,
    pub(in crate::codegen) module: &'a Module<'ctx>,
    pub(in crate::codegen) functions: HashMap<InternedStr, FunctionValue<'ctx>>,
    pub(in crate::codegen) variables: HashMap<InternedStr, PointerValue<'ctx>>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    #[inline(always)]
    fn get_next_block(&self) -> Option<BasicBlock<'ctx>> {
        self.builder().get_insert_block()?.get_next_basic_block()
    }

    #[inline(always)]
    fn builder(&self) -> &Builder<'ctx> {
        self.builder.as_ref().unwrap()
    }

    #[inline(always)]
    fn get_block_by_name(&self, name: &str) -> Option<BasicBlock<'ctx>> {
        self.fn_value()
            .get_basic_block_iter()
            .find(|block| block.get_name().to_str().unwrap() == name)
    }

    #[inline(always)]
    fn last_block(&self) -> BasicBlock<'ctx> {
        self.fn_value().get_last_basic_block().unwrap()
    }

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

        self.builder().position_at_end(entry);
        self.fn_value_opt = Some(context_function);
        self.variables.reserve(function.parameters.len());

        for (i, arg) in context_function.get_param_iter().enumerate() {
            let arg_name = function.parameters[i].ident.clone();
            let allocation = self.create_entry_block_allocation(arg.get_type(), &arg_name);

            self.builder().build_store(allocation, arg).unwrap();

            self.variables.insert(arg_name, allocation);
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

    fn compile_mlir_basic_block(&mut self, mlir_bb: &MlirBasicBlock) {
        for stmt in &mlir_bb.stmts {
            let mut block_has_jumped = false;
            match stmt {
                MlirStmt::Expression(expression) => {
                    self.compile_expression_statement(expression);
                }

                MlirStmt::VariableDeclaration(var) => {
                    self.compile_variable_declaration_statement(var);
                }

                MlirStmt::Goto(label) => {
                    block_has_jumped = true;
                    self.compile_goto(label);
                }

                MlirStmt::CondGoto(condition, then, _else) => {
                    block_has_jumped = true;
                    self.compile_cond_goto(condition, then, _else);
                }

                MlirStmt::Return(expression) => {
                    block_has_jumped = true;
                    self.compile_return_statement(expression);
                }

                MlirStmt::Block(_) | MlirStmt::Label(_) => {
                    unreachable!("Blocks and labels must not exist at this stage in the process.")
                }
            }
            if !block_has_jumped {
                let next_block_opt = self
                    .builder()
                    .get_insert_block()
                    .unwrap()
                    .get_next_basic_block();
                if let Some(next_block) = next_block_opt {
                    self.builder().build_unconditional_branch(next_block);
                }
            }
        }
    }

    fn compile_expression_statement(&mut self, expression: &MlirExpr) {
        todo!()
    }

    fn compile_variable_declaration_statement(&mut self, var: &MlirVariable) {
        todo!()
    }

    fn compile_goto(&self, label: &InternedStr) {
        let goto_block = self.get_block_by_name(label).unwrap();
        self.builder().build_unconditional_branch(goto_block);
    }

    fn compile_cond_goto(&mut self, condition: &MlirExpr, then: &InternedStr, _else: &InternedStr) {
        let condition = match self.compile_expression(condition) {
            BasicValueEnum::IntValue(int_value) => int_value,
            _ => panic!(
                "Cannot build conditional comparison to zero for non IntValue type: '{:?}'",
                condition
            ),
        };
        let then_block = self.get_block_by_name(then).unwrap();
        let else_block = self.get_block_by_name(_else).unwrap();
        self.builder()
            .build_conditional_branch(condition, then_block, else_block);
    }

    fn compile_return_statement(&mut self, expression_opt: &Option<MlirExpr>) {
        let expression = expression_opt
            .as_ref()
            .map(|expression| self.compile_expression(expression));
        // '.as_ref()' wasn't satisfying the lifetime requirements, so I did it explicitly.
        let trait_ref_expression: Option<&dyn BasicValue<'ctx>> =
            if let Some(expression) = expression.as_ref() {
                Some(expression)
            } else {
                None
            };
        let _return = self.builder().build_return(trait_ref_expression).unwrap();
    }

    fn compile_expression(&mut self, expression: &MlirExpr) -> BasicValueEnum<'ctx> {
        match &*expression.kind {
            MlirExprKind::Literal(literal) => self.compile_literal(literal),
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

    #[rustfmt::skip]
    fn compile_literal(&self, literal: &MlirLiteral) -> BasicValueEnum<'ctx> {
        match literal {
            MlirLiteral::Char(char) => {
                self.context.i8_type().const_int(*char as u64, true).into()
            }
            MlirLiteral::UChar(char) => {
                self.context.i8_type().const_int(*char as u64, false).into()
            }
            MlirLiteral::Int(int) => {
                self.context.i32_type().const_int(*int as u64, true).into()
            }
            MlirLiteral::UInt(int) => {
                self.context.i32_type().const_int(*int as u64, false).into()
            }
            MlirLiteral::Long(long) => {
                self.context.i64_type().const_int(*long as u64, true).into()
            }
            MlirLiteral::ULong(long) => {
                self.context.i64_type().const_int(*long as u64, false).into()
            }
            MlirLiteral::Float(float) => {
                self.context.f32_type().const_float(*float as f64).into()
            }
            MlirLiteral::Double(double) => {
                self.context.f64_type().const_float(*double).into()
            }
            MlirLiteral::String(bytes) => {
                let bytes = bytes
                    .iter()
                    .map(|byte| {
                        self.context.i8_type().const_int(*byte as u64, false)
                    })
                    .collect::<Vec<_>>();
                self.context.i8_type().const_array(&bytes).into()
            }
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
