use inkwell::values::BasicValue;

use crate::codegen::{Compiler, MlirBasicBlock};
use crate::data::mlir::{MlirExpr, MlirStmt};
use crate::util::str_intern::InternedStr;

impl<'a, 'mlir, 'ctx> Compiler<'a, 'mlir, 'ctx> {
    pub fn compile_mlir_basic_block(&mut self, mlir_bb: &MlirBasicBlock<'mlir>) {
        for stmt in &mlir_bb.stmts {
            self.block_has_jumped = false;
            match stmt {
                MlirStmt::Expression(expression) => {
                    self.compile_expression_statement(expression);
                }

                MlirStmt::VariableDeclaration(var) => {
                    self.compile_variable_declaration(var);
                }

                MlirStmt::Goto(label) => {
                    self.block_has_jumped = true;
                    self.compile_goto(label);
                }

                MlirStmt::CondGoto(condition, then, _else) => {
                    self.block_has_jumped = true;
                    self.compile_cond_goto(condition, then, _else);
                }

                MlirStmt::Return(expression) => {
                    self.block_has_jumped = true;
                    self.compile_return_statement(expression);
                }

                MlirStmt::Block(_) | MlirStmt::Label(_) => {
                    unreachable!("Blocks and labels must not exist at this stage in the process.")
                }
            }
        }
    }

    #[inline(always)]
    fn compile_expression_statement(&mut self, expression: &MlirExpr) {
        self.compile_expression(expression);
    }

    #[inline(always)]
    fn compile_goto(&mut self, label: &InternedStr) {
        let goto_block = self.get_block_by_name(label);
        self.builder().build_unconditional_branch(goto_block);
    }

    #[inline(always)]
    fn compile_cond_goto(&mut self, condition: &MlirExpr, then: &InternedStr, _else: &InternedStr) {
        let condition = self.compile_expression(condition).into_int_value();
        let i1_type = self.context.bool_type();
        let condition = self
            .builder()
            .build_int_cast(condition, i1_type, "conv_to_i1_type")
            .unwrap();
        let then_block = self.get_block_by_name(then);
        let else_block = self.get_block_by_name(_else);
        self.builder()
            .build_conditional_branch(condition, then_block, else_block);
    }

    #[inline(always)]
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
