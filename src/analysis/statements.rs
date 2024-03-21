use crate::analysis::hlir::{
    HlirBlock, HlirExpr, HlirExprKind, HlirLiteral, HlirStmt, HlirType, HlirTypeDecl, HlirTypeKind,
};
use crate::analysis::GlobalValidator;
use crate::parser::ast::{Block, Expression, Statement, VariableDeclaration};
use crate::util::Locatable;
use std::env::var;

impl GlobalValidator {
    pub(super) fn validate_block(&mut self, block: &Locatable<Block>) -> Result<HlirBlock, ()> {
        let mut statements = Vec::new();
        for raw_stmt in &block.0 {
            if let Some(stmt) = self.validate_statement(raw_stmt)? {
                statements.push(stmt);
            }
        }
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
            Statement::Declaration(var_dec) => Ok(Some(HlirStmt::VariableDeclaration(
                self.validate_variable(var_dec)?,
            ))),
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
            Statement::Return(value) => self.validate_return_statement(value),
            Statement::Continue => Ok(Some(HlirStmt::Continue)),
            Statement::Break => Ok(Some(HlirStmt::Break)),
            Statement::Empty => Ok(None),
        }
    }
    fn validate_if_statement(
        &mut self,
        condition: &Locatable<Expression>,
        then: &Locatable<Statement>,
        otherwise: &Option<Box<Locatable<Statement>>>,
    ) -> Result<Option<HlirStmt>, ()> {
        let condition = self.validate_expression(condition)?;
        let then = self.validate_statement(then)?;
        if then.is_none() {
            return Ok(None);
        }
        let then = Box::new(then.unwrap());
        let otherwise = if let Some(otherwise) = otherwise {
            let otherwise = self.validate_statement(otherwise)?;
            if otherwise.is_none() {
                return Ok(None);
            }
            Some(Box::new(otherwise.unwrap()))
        } else {
            None
        };
        Ok(Some(HlirStmt::If {
            condition,
            then,
            otherwise,
        }))
    }

    fn validate_while_loop_statement(
        &mut self,
        condition: &Locatable<Expression>,
        body: &Locatable<Statement>,
    ) -> Result<Option<HlirStmt>, ()> {
        let condition = self.validate_expression(condition)?;
        let body = self.validate_statement(body)?;
        if body.is_none() {
            return Ok(None);
        }
        let body = Box::new(body.unwrap());
        Ok(Some(HlirStmt::While { condition, body }))
    }

    fn validate_return_statement(
        &mut self,
        value: &Option<Locatable<Expression>>,
    ) -> Result<Option<HlirStmt>, ()> {
        let value = if let Some(value) = value {
            Some(self.validate_expression(value)?)
        } else {
            None
        };
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
        body: &Box<Locatable<Statement>>,
    ) -> Result<Option<HlirStmt>, ()> {
        let mut block_body = Vec::new();
        if let Some(initializer) = initializer {
            let var_dec = self.validate_variable(initializer)?;
            let var_stmt = HlirStmt::VariableDeclaration(var_dec);
            block_body.push(var_stmt);
        }
        let condition = if let Some(condition) = condition {
            self.validate_expression(condition)?
        } else {
            HlirExpr {
                kind: Box::new(HlirExprKind::Literal(HlirLiteral::Int(1))),
                ty: HlirType {
                    kind: HlirTypeKind::Int(false),
                    decl: HlirTypeDecl::Basic,
                },
                is_lval: false,
            }
        };
        let body_stmt = self.validate_statement(body)?;
        if body_stmt.is_none() {
            return Ok(None);
        }
        let mut body = Vec::new();
        body.push(body_stmt.unwrap());
        if let Some(post_loop) = post_loop {
            let post_loop = self.validate_expression(post_loop)?;
            let post_loop_stmt = HlirStmt::Expression(post_loop);
            body.push(post_loop_stmt);
        }
        let body = Box::new(HlirStmt::Block(HlirBlock(body)));
        let as_while_loop = HlirStmt::While { condition, body };
        block_body.push(as_while_loop);

        Ok(Some(HlirStmt::Block(HlirBlock(block_body))))
    }
}
