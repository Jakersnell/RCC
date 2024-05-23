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

use crate::data::mlir::{
    MidLevelIR, MlirBlock, MlirExpr, MlirExprKind, MlirFunction, MlirLiteral, MlirStmt, MlirType,
    MlirTypeDecl, MlirTypeKind, MlirVariable, VOID_TYPE,
};
use crate::data::symbols::BUILTINS;
use crate::util::str_intern;
use crate::util::str_intern::InternedStr;

mod binary_expressions;
mod expressions;
mod statements;

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
    pub(in crate::codegen) variables: HashMap<usize, PointerValue<'ctx>>,
    pub(in crate::codegen) fn_value_opt: Option<FunctionValue<'ctx>>,
    pub(in crate::codegen) ptr_types: HashMap<PointerValue<'ctx>, BasicTypeEnum<'ctx>>,
}

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
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

    pub(in crate::codegen) fn compile_expression(
        &mut self,
        expression: &MlirExpr,
    ) -> BasicValueEnum<'ctx> {
        match &*expression.kind {
            MlirExprKind::Literal(literal) => self.compile_literal(literal),
            MlirExprKind::Variable(id) => self.compile_variable_access(*id),
            MlirExprKind::PostIncrement(expr) => self.compile_post_increment(expr),
            MlirExprKind::PostDecrement(expr) => self.compile_post_decrement(expr),
            MlirExprKind::Negate(expr) => self.compile_negate(expr),
            MlirExprKind::LogicalNot(expr) => self.compile_logical_not(expr),
            MlirExprKind::BitwiseNot(_) => todo!(),
            MlirExprKind::Deref(expr) => self.compile_deref(expr),
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
                self.context.i64_type().const_int(*long, false).into()
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

    fn compile_variable_access(&mut self, id: usize) -> BasicValueEnum<'ctx> {
        BasicValueEnum::from(
            *self
                .variables
                .get(&id)
                .expect("Invalid variable accesses must be handled in Analysis."),
        )
    }

    fn compile_post_increment(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_post_inc_or_dec(expr, true)
    }

    fn compile_post_decrement(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        self.compile_post_inc_or_dec(expr, false)
    }

    fn compile_post_inc_or_dec(&mut self, expr: &MlirExpr, inc: bool) -> BasicValueEnum<'ctx> {
        debug_assert!(expr.is_lval);
        let expr = self.compile_expression(expr);

        let ptr = match expr {
            BasicValueEnum::PointerValue(ptr) => ptr,
            _ => panic!("Expected BasicValueEnum::PointerValue, found '{:?}'", expr),
        };

        let ptr_type = self.get_pointer_type(&ptr);
        let ptr_value = self
            .builder()
            .build_load(ptr_type, ptr, "post_inc_load_value")
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

    fn compile_negate(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = self.compile_expression(expr);
        match expr {
            BasicValueEnum::IntValue(int) => {
                BasicValueEnum::from(self.builder().build_int_neg(int, "int-neg").unwrap())
            }
            BasicValueEnum::FloatValue(float) => {
                BasicValueEnum::from(self.builder().build_float_neg(float, "float-neg").unwrap())
            }
            _ => panic!("Value cannot be negated: {:?}", expr),
        }
    }

    fn compile_logical_not(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = self.compile_expression(expr);
        let int_val = match expr {
            BasicValueEnum::IntValue(int_val) => {
                let zero = self.context.i8_type().const_int(0, false);
                self.builder()
                    .build_int_compare(IntPredicate::EQ, int_val, zero, "int_cmp_zero_logical_not")
                    .unwrap()
            }

            BasicValueEnum::FloatValue(float_val) => {
                let zero = self.context.f32_type().const_float(0.0);
                self.builder()
                    .build_float_compare(
                        FloatPredicate::OEQ,
                        float_val,
                        zero,
                        "float_cmp_zero_logical_not",
                    )
                    .unwrap()
            }
            _ => panic!(),
        };

        BasicValueEnum::from(int_val)
    }

    fn compile_deref(&mut self, expr: &MlirExpr) -> BasicValueEnum<'ctx> {
        let expr = match self.compile_expression(expr) {
            BasicValueEnum::PointerValue(ptr) => ptr,
            unexpected => panic!(
                "Expected PointerValue, but found '{:?}'",
                unexpected.get_type()
            ),
        };
        self.deref_pointer_value(expr)
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
