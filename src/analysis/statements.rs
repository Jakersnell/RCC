use crate::analysis::hlir::HlirTypeDecl::Basic;
use crate::analysis::hlir::HlirTypeKind::Void;
use crate::analysis::hlir::{
    HlirBlock, HlirExpr, HlirExprKind, HlirFunction, HlirLiteral, HlirStmt, HlirType, HlirTypeDecl,
    HlirTypeKind,
};
use crate::analysis::GlobalValidator;
use crate::parser::ast::{Block, Expression, Statement, VariableDeclaration};
use crate::util::error::CompilerError;
use crate::util::{str_intern, Locatable, Span};
use std::env::var;
use std::fmt::format;

fn invert_condition(expr: Locatable<HlirExpr>) -> Locatable<HlirExpr> {
    let ty = expr.ty.clone();
    expr.location.into_locatable(HlirExpr {
        span: expr.location,
        kind: Box::new(HlirExprKind::LogicalNot(expr.value)),
        is_lval: false,
        ty,
    })
}

impl GlobalValidator {
    pub(super) fn validate_block(&mut self, block: &Locatable<Block>) -> Result<HlirBlock, ()> {
        self.push_scope();
        let mut statements = Vec::new();
        for raw_stmt in &block.0 {
            if let Some(stmt) = self.validate_statement(raw_stmt)? {
                statements.push(stmt);
            }
        }
        self.pop_scope();
        Ok(HlirBlock(statements))
    }

    pub(super) fn validate_statement(
        &mut self,
        stmt: &Locatable<Statement>,
    ) -> Result<Option<HlirStmt>, ()> {
        match &stmt.value {
            Statement::Expression(expr) => Ok(Some(HlirStmt::Expression(
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
            Statement::Block(block) => Ok(Some(HlirStmt::Block(self.validate_block(block)?))),
            Statement::Return(value) => self.validate_return_statement(value, stmt.location),
            Statement::Continue => self.validate_continue_statement(stmt.location),
            Statement::Break => self.validate_break_statement(stmt.location),
            Statement::Empty => Ok(None),
        }
    }

    fn validate_continue_statement(&mut self, span: Span) -> Result<Option<HlirStmt>, ()> {
        self.loop_label_stack
            .iter()
            .last()
            .map(|label| Some(HlirStmt::Goto(label.clone())))
            .ok_or_else(|| {
                self.report_error(CompilerError::ContinueWithoutLoop(span));
            })
    }

    fn validate_break_statement(&mut self, span: Span) -> Result<Option<HlirStmt>, ()> {
        self.loop_label_stack
            .iter()
            .last()
            .map(|label| Some(HlirStmt::Goto(str_intern::intern(format!("{}_end", label)))))
            .ok_or_else(|| {
                self.report_error(CompilerError::BreakWithoutLoop(span));
            })
    }

    fn validate_variable_declaration_statement(
        &mut self,
        var_dec: &Locatable<VariableDeclaration>,
    ) -> Result<Option<HlirStmt>, ()> {
        let span = var_dec.location;
        let var_dec = self.validate_variable_declaration(var_dec)?;
        self.add_variable_to_scope(&var_dec, span)?;
        Ok(Some(HlirStmt::VariableDeclaration(var_dec)))
    }

    fn validate_if_statement(
        &mut self,
        condition: &Locatable<Expression>,
        then: &Locatable<Statement>,
        otherwise: &Option<Box<Locatable<Statement>>>,
    ) -> Result<Option<HlirStmt>, ()> {
        let start_label = str_intern::intern(format!("label_{}", super::create_label()));
        let else_label = str_intern::intern(format!("{}_else", start_label));
        let end_label = str_intern::intern(format!("{}_end", start_label));
        let mut block = Vec::new();

        let condition = condition
            .location
            .into_locatable(self.validate_expression(condition)?);
        let jump_to = if otherwise.is_some() {
            else_label.clone()
        } else {
            end_label.clone()
        };
        let conditional_goto =
            HlirStmt::ConditionalGoto(invert_condition(condition).value, jump_to);
        block.push(conditional_goto);

        let then_span = then.location;
        if let Some(then) = self.validate_statement(then)? {
            block.push(then);
        }

        let else_label = HlirStmt::Label(else_label);
        block.push(else_label);

        if let Some(otherwise) = otherwise {
            if let Some(otherwise) = self.validate_statement(otherwise)? {
                block.push(otherwise);
            }
        }

        let end_label = HlirStmt::Label(end_label);
        block.push(end_label);

        Ok(Some(HlirStmt::Block(HlirBlock(block))))
    }

    fn validate_while_loop_statement(
        &mut self,
        condition: &Locatable<Expression>,
        body: &Locatable<Statement>,
    ) -> Result<Option<HlirStmt>, ()> {
        self.push_scope();

        let mut block = Vec::new();
        let label_string = str_intern::intern(format!("label_{}", super::create_label()));
        let label_string_end = str_intern::intern(format!("{}_end", label_string));

        self.loop_label_stack.push_front(label_string.clone());
        let label = HlirStmt::Label(label_string.clone());
        block.push(label);

        let condition = condition
            .location
            .into_locatable(self.validate_expression(condition)?);
        let goto_end =
            HlirStmt::ConditionalGoto(invert_condition(condition).value, label_string_end.clone());
        block.push(goto_end);

        if let Some(body) = self.validate_statement(body)? {
            block.push(body);
        }

        let jump_to_start = HlirStmt::Goto(label_string);
        block.push(jump_to_start);

        let end_label = HlirStmt::Label(label_string_end);
        block.push(end_label);

        self.loop_label_stack.pop_front();
        self.pop_scope();

        Ok(Some(HlirStmt::Block(HlirBlock(block))))
    }

    fn validate_return_statement(
        &mut self,
        value: &Option<Locatable<Expression>>,
        span: Span,
    ) -> Result<Option<HlirStmt>, ()> {
        let value = if let Some(value) = value {
            Some(self.validate_expression(value)?)
        } else {
            None
        };
        static NONE_TYPE: HlirType = HlirType {
            kind: Void,
            decl: Basic,
        };
        let function_ty = self.return_ty.as_ref().unwrap_or(&NONE_TYPE);
        let value_ty = value.as_ref().map(|expr| &expr.ty).unwrap_or(&NONE_TYPE);
        if function_ty != value_ty {
            self.report_error(CompilerError::InvalidReturnType(
                function_ty.to_string(),
                value_ty.to_string(),
                span,
            ));
        }
        Ok(Some(HlirStmt::Return(value)))
    }

    /*
    Breaks a for loop into a block containing a potential initializer and a while loop
     */
    fn validate_for_loop(
        &mut self,
        initializer: &Option<Locatable<VariableDeclaration>>,
        condition: &Option<Locatable<Expression>>,
        post_loop: &Option<Locatable<Expression>>,
        body: &Locatable<Statement>,
    ) -> Result<Option<HlirStmt>, ()> {
        self.push_scope();
        let loop_start_label = str_intern::intern(format!("loop_{}", super::create_label()));
        let loop_end = str_intern::intern(format!("{}_end", loop_start_label));
        let mut block = Vec::new();

        if let Some(initializer) = initializer {
            let var_dec = self.validate_variable_declaration(initializer)?;
            self.add_variable_to_scope(&var_dec, initializer.location);
            let var_stmt = HlirStmt::VariableDeclaration(var_dec);
            block.push(var_stmt);
        }

        self.loop_label_stack.push_front(loop_start_label.clone());
        let start_label = HlirStmt::Label(loop_start_label.clone());
        block.push(start_label);

        if let Some(condition) = condition {
            let condition = condition
                .location
                .into_locatable(self.validate_expression(condition)?);
            let condition =
                HlirStmt::ConditionalGoto(invert_condition(condition).value, loop_end.clone());
            block.push(condition);
        }

        if let Some(body) = self.validate_statement(body)? {
            block.push(body);
        }

        if let Some(post_loop) = post_loop {
            let post_loop = self.validate_expression(post_loop)?;
            let post_loop_stmt = HlirStmt::Expression(post_loop);
            block.push(post_loop_stmt);
        }

        let goto_start = HlirStmt::Goto(loop_start_label);
        block.push(goto_start);

        let end_label = HlirStmt::Label(loop_end);
        block.push(end_label);

        self.pop_scope();
        self.loop_label_stack.pop_front();

        Ok(Some(HlirStmt::Block(HlirBlock(block))))
    }
}
