use crate::data::ast::{Block, Statement};
use crate::data::tokens::{Keyword, Symbol, Token};
use crate::parser::{Parser, ParseResult};
use crate::parser::macros::{confirm, is};
use crate::util::error::CompilerError;
use crate::util::Locatable;

impl<L> Parser<L>
where
    L: Iterator<Item = Locatable<Token>>,
{
    pub fn parse_compound_statement(&mut self) -> ParseResult<Locatable<Block>> {
        let location = self.current_span()?;
        confirm!(self, consume, Token::Symbol(Symbol::OpenCurly) => (), "{")?;
        let mut body = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseCurly)) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }
        confirm!(self, consume, Token::Symbol(Symbol::CloseCurly) => (), "}")?;
        let location = location.merge(self.last_span);
        Ok(Locatable::new(location, Block(body)))
    }

    pub fn parse_statement(&mut self) -> ParseResult<Locatable<Statement>> {
        self.check_for_eof("statement")?;
        let location = self.current_span()?;
        match self.current.as_ref().unwrap().value {
            Token::Symbol(Symbol::Semicolon) => Ok(self.consume()?.map(|_| Statement::Empty)),
            Token::Keyword(Keyword::Continue) => {
                let stmt = self.consume()?.map(|_| Statement::Continue);
                self.confirm_semicolon()?;
                Ok(stmt)
            }
            Token::Keyword(Keyword::Break) => {
                let stmt = self.consume()?.map(|_| Statement::Break);
                self.confirm_semicolon()?;
                Ok(stmt)
            }
            Token::Symbol(Symbol::OpenCurly) => {
                let block = self.parse_compound_statement()?;
                let location = block.location;
                Ok(Locatable::new(location, Statement::Block(block)))
            }
            Token::Keyword(keyword) if keyword.is_for_type() => {
                let dec = self.parse_declaration()?;
                let variable_declaration = self.parse_variable_declaration(dec)?;
                let res = self.confirm_semicolon()?;
                let location = variable_declaration.location.merge(self.last_span);
                Ok(Locatable::new(
                    location,
                    Statement::Declaration(variable_declaration),
                ))
            }
            Token::Keyword(Keyword::Return) => {
                self.advance()?;
                let stmt = if is!(self, current, Token::Symbol(Symbol::Semicolon)) {
                    Statement::Return(None)
                } else {
                    let expr = self.parse_binary_expression(None)?;
                    confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                    Statement::Return(Some(expr))
                };
                let location = location.merge(self.last_span);
                Ok(Locatable::new(location, stmt))
            }
            Token::Keyword(Keyword::If) => {
                self.advance()?;
                confirm!(self, consume, Token::Symbol(Symbol::OpenParen) => (), "(")?;
                let condition = self.parse_binary_expression(None)?;
                confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
                let stmt = Box::new(self.parse_statement()?);
                let else_stmt = if is!(self, current, Token::Keyword(Keyword::Else)) {
                    self.advance()?;
                    Some(Box::new(self.parse_statement()?))
                } else {
                    None
                };
                let location = location.merge(self.last_span);
                Ok(Locatable::new(
                    location,
                    Statement::If(condition, stmt, else_stmt),
                ))
            }
            Token::Keyword(Keyword::While) => {
                self.advance()?;
                confirm!(self, consume, Token::Symbol(Symbol::OpenParen) => (), "(")?;
                let condition = self.parse_binary_expression(None)?;
                confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
                let stmt = Box::new(self.parse_statement()?);
                let location = location.merge(stmt.location);
                Ok(Locatable::new(location, Statement::While(condition, stmt)))
            }
            Token::Keyword(Keyword::For) => {
                self.advance()?;
                confirm!(self, consume, Token::Symbol(Symbol::OpenParen) => (), "(")?;
                let initializer = if is!(self, current, Token::Symbol(Symbol::Semicolon)) {
                    None
                } else {
                    let dec = self.parse_declaration()?;
                    Some(self.parse_variable_declaration(dec)?)
                };
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                let condition = if is!(self, current, Token::Symbol(Symbol::Semicolon)) {
                    None
                } else {
                    Some(self.parse_binary_expression(None)?)
                };
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                let after_loop = if is!(self, current, Token::Symbol(Symbol::CloseParen)) {
                    None
                } else {
                    Some(self.parse_binary_expression(None)?)
                };
                confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
                let stmt = Box::new(self.parse_statement()?);
                let location = location.merge(stmt.location);
                Ok(Locatable::new(
                    location,
                    Statement::For(initializer, condition, after_loop, stmt),
                ))
            }
            Token::Keyword(Keyword::Else) => {
                let location = self.consume()?.location;
                self.report_error(CompilerError::ElseWithNoIf(location));
                Err(())
            }
            _ => {
                let stmt = self
                    .parse_binary_expression(None)
                    .map(Statement::Expression)?;
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), "Expression statements must be terminated by a semicolon.")?;
                let location = location.merge(self.last_span);
                Ok(Locatable::new(location, stmt))
            }
        }
    }
}
