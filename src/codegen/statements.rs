use inkwell::values::{BasicValue, BasicValueEnum};

use crate::codegen::{Compiler, MlirBasicBlock};
use crate::data::mlir::{MlirExpr, MlirStmt, MlirVariable};
use crate::util::str_intern::InternedStr;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub(in crate::codegen) fn compile_mlir_basic_block(&mut self, mlir_bb: &MlirBasicBlock) {
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
}
