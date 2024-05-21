use crate::analysis::Analyzer;
use crate::data::ast::{Block, Expression, Statement, VariableDeclaration};
use crate::data::mlir::{MlirBlock, MlirExpr, MlirStmt, SIGNED_INT_TYPE, VOID_TYPE};
use crate::util::{Locatable, Span, str_intern};
use crate::util::error::CompilerError;

impl Analyzer {
    #[inline(always)]
    fn validate_conditional(&mut self, expression: &Expression) -> Result<MlirExpr, ()> {
        let cond = self.validate_expression(expression)?;
        let cond_span = cond.span;
        Ok(self.implicit_cast(cond, SIGNED_INT_TYPE, cond_span))
    }

    pub(super) fn validate_block(&mut self, block: &Locatable<Block>) -> Result<MlirBlock, ()> {
        self.push_scope();
        let mut statements = Vec::new();
        for raw_stmt in &block.0 {
            if let Some(stmt) = self.validate_statement(raw_stmt)? {
                statements.push(stmt);
            }
        }
        self.pop_scope();
        Ok(MlirBlock(statements))
    }

    pub(super) fn validate_statement(
        &mut self,
        stmt: &Locatable<Statement>,
    ) -> Result<Option<MlirStmt>, ()> {
        match &stmt.value {
            Statement::Expression(expr) => Ok(Some(MlirStmt::Expression(
                self.validate_expression(&expr.value)?,
            ))),
            Statement::Declaration(var_dec) => {
                self.validate_variable_declaration_statement(var_dec)
            }
            Statement::If(condition, then, otherwise) => {
                self.validate_if_statement(condition, then, otherwise)
            }
            Statement::While(condition, body) => {
                self.validate_while_loop_statement(condition, body)
            }
            Statement::For(initializer, condition, post_loop, body) => {
                self.validate_for_loop(initializer, condition, post_loop, body)
            }
            Statement::Block(block) => Ok(Some(MlirStmt::Block(self.validate_block(block)?))),
            Statement::Return(value) => self.validate_return_statement(value, stmt.location),
            Statement::Continue => self.validate_continue_statement(stmt.location),
            Statement::Break => self.validate_break_statement(stmt.location),
            Statement::Empty => Ok(None),
        }
    }

    fn validate_continue_statement(&mut self, span: Span) -> Result<Option<MlirStmt>, ()> {
        self.loop_label_stack
            .iter()
            .last()
            .map(|label| Some(MlirStmt::Goto(label.clone())))
            .ok_or_else(|| {
                self.report_error(CompilerError::ContinueWithoutLoop(span));
            })
    }

    fn validate_break_statement(&mut self, span: Span) -> Result<Option<MlirStmt>, ()> {
        self.loop_label_stack
            .iter()
            .last()
            .map(|label| Some(MlirStmt::Goto(str_intern::intern(format!("{}_end", label)))))
            .ok_or_else(|| {
                self.report_error(CompilerError::BreakWithoutLoop(span));
            })
    }

    fn validate_variable_declaration_statement(
        &mut self,
        var_dec: &Locatable<VariableDeclaration>,
    ) -> Result<Option<MlirStmt>, ()> {
        let span = var_dec.location;
        let var_dec = self.validate_variable_declaration(var_dec)?;
        self.add_variable_to_scope(&var_dec, span)?;
        Ok(Some(MlirStmt::VariableDeclaration(var_dec)))
    }

    fn validate_if_statement(
        &mut self,
        condition: &Locatable<Expression>,
        then: &Locatable<Statement>,
        otherwise: &Option<Box<Locatable<Statement>>>,
    ) -> Result<Option<MlirStmt>, ()> {
        let start_label = str_intern::intern(format!("if_{}", super::create_label()));
        let then_label = str_intern::intern(format!("{}_then", start_label));
        let else_label = str_intern::intern(format!("{}_else", start_label));
        let end_label = str_intern::intern(format!("{}_end", start_label));
        let mut block = Vec::new();

        let condition = self.validate_conditional(condition)?;
        let conditional_goto = MlirStmt::CondGoto(condition, then_label.clone(), else_label.clone());
        block.push(conditional_goto);

        block.push(MlirStmt::Label(then_label));

        if let Some(then) = self.validate_statement(then)? {
            block.push(then);
            let goto_end = MlirStmt::Goto(end_label.clone());
            block.push(goto_end);
        }

        block.push(MlirStmt::Label(else_label));

        if let Some(otherwise) = otherwise {
            if let Some(otherwise) = self.validate_statement(otherwise)? {
                block.push(otherwise);
            }
        }

        block.push(MlirStmt::Label(end_label));

        Ok(Some(MlirStmt::Block(MlirBlock(block))))
    }

    fn validate_while_loop_statement(
        &mut self,
        condition: &Locatable<Expression>,
        body: &Locatable<Statement>,
    ) -> Result<Option<MlirStmt>, ()> {
        self.push_scope();

        let mut block = Vec::new();
        let label_string = str_intern::intern(format!("loop_{}", super::create_label()));
        let label_string_then = str_intern::intern(format!("{}_body", label_string));
        let label_string_end = str_intern::intern(format!("{}_end", label_string));

        self.loop_label_stack.push_front(label_string.clone());
        let label = MlirStmt::Label(label_string.clone());
        block.push(label);

        let condition = self.validate_conditional(condition)?;
        let branch = MlirStmt::CondGoto(condition, label_string_then.clone(), label_string_end.clone());
        block.push(branch);

        let body_label = MlirStmt::Label(label_string_then);
        block.push(body_label);

        if let Some(body) = self.validate_statement(body)? {
            block.push(body);
        }

        let jump_to_start = MlirStmt::Goto(label_string);
        block.push(jump_to_start);

        let end_label = MlirStmt::Label(label_string_end);
        block.push(end_label);

        self.loop_label_stack.pop_front();
        self.pop_scope();

        Ok(Some(MlirStmt::Block(MlirBlock(block))))
    }

    fn validate_return_statement(
        &mut self,
        value: &Option<Locatable<Expression>>,
        span: Span,
    ) -> Result<Option<MlirStmt>, ()> {
        let function_ty = self.return_ty.as_ref().unwrap_or(&VOID_TYPE).clone();
        let value = if let Some(value) = value {
            let value_mlir = self.validate_expression(value)?;
            let casted_value_mlir = self.implicit_cast(value_mlir, function_ty.clone(), span).fold();
            Some(casted_value_mlir)
        } else {
            None
        };
        let value_ty = value
            .as_ref()
            .map(|expr| &expr.ty)
            .unwrap_or(&VOID_TYPE)
            .clone();
        if function_ty != value_ty {
            self.report_error(CompilerError::InvalidReturnType(
                function_ty.to_string(),
                value_ty.to_string(),
                span,
            ));
        }
        Ok(Some(MlirStmt::Return(value)))
    }

    fn validate_for_loop(
        &mut self,
        initializer: &Option<Locatable<VariableDeclaration>>,
        condition: &Option<Locatable<Expression>>,
        post_loop: &Option<Locatable<Expression>>,
        body: &Locatable<Statement>,
    ) -> Result<Option<MlirStmt>, ()> {
        self.push_scope();
        let loop_start_label = str_intern::intern(format!("loop_{}", super::create_label()));
        let loop_body_label = str_intern::intern(format!("{}_body", loop_start_label));
        let loop_end_label = str_intern::intern(format!("{}_end", loop_start_label));
        let mut block = Vec::new();

        if let Some(initializer) = initializer {
            let var_dec = self.validate_variable_declaration(initializer)?;
            self.add_variable_to_scope(&var_dec, initializer.location);
            let var_stmt = MlirStmt::VariableDeclaration(var_dec);
            block.push(var_stmt);
        }

        self.loop_label_stack.push_front(loop_start_label.clone());
        block.push(MlirStmt::Label(loop_start_label.clone()));

        if let Some(condition) = condition {
            let condition = self.validate_conditional(condition)?;
            let condition = MlirStmt::CondGoto(condition, loop_body_label.clone(), loop_end_label.clone());
            block.push(condition);
        }

        block.push(MlirStmt::Label(loop_body_label));

        if let Some(body) = self.validate_statement(body)? {
            block.push(body);
        }

        if let Some(post_loop) = post_loop {
            let post_loop = self.validate_expression(post_loop)?;
            let post_loop_stmt = MlirStmt::Expression(post_loop);
            block.push(post_loop_stmt);
        }

        let goto_start = MlirStmt::Goto(loop_start_label);
        block.push(goto_start);

        let end_label = MlirStmt::Label(loop_end_label);
        block.push(end_label);

        self.pop_scope();
        self.loop_label_stack.pop_front();

        Ok(Some(MlirStmt::Block(MlirBlock(block))))
    }
}
