use super::macros::*;
use crate::lexer::tokens::{Symbol, Token};
use crate::lexer::LexResult;
use crate::parser::ast::{BinaryOp, Expression, PostfixOp, TypeOrExpression, UnaryOp};
use crate::parser::{ParseResult, Parser};
use crate::util::error::CompilerError;
use crate::util::Locatable;
use arcstr::ArcStr;

impl<L> Parser<L>
where
    L: Iterator<Item = LexResult>,
{
    pub(super) fn parse_initializer(&mut self) -> ParseResult<Locatable<Expression>> {
        if is!(self, current, Token::Symbol(Symbol::OpenCurly)) {
            let location = self.current_span()?;
            self.advance()?;
            let mut contents = Vec::new();
            while !is!(self, current, Token::Symbol(Symbol::CloseCurly)) {
                let expr = self.parse_initializer()?;
                contents.push(expr);
                if is!(self, current, Token::Symbol(Symbol::Comma)) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            confirm!(self, consume, Token::Symbol(Symbol::CloseCurly) => (), "}")?;
            let location = location.merge(self.current_span()?);
            Ok(Locatable::new(
                location,
                Expression::ArrayInitializer(contents),
            ))
        } else {
            self.parse_binary_expression(None)
        }
    }

    pub(super) fn parse_binary_expression(
        &mut self,
        parent_precedence: Option<u8>,
    ) -> ParseResult<Locatable<Expression>> {
        let parent_precedence = parent_precedence.unwrap_or(0);
        let mut left = self.parse_prefix_unary_expression()?;

        while let Ok(bin_op) = self.match_binary_op() {
            let precedence = bin_op.value.precedence();
            if precedence == 0 || precedence <= parent_precedence {
                break;
            }
            self.advance()?;
            let precedence = if let BinaryOp::Assign(op) = &bin_op.value {
                precedence - 1
            } else {
                precedence
            };
            let right = self.parse_binary_expression(Some(precedence))?;
            let location = left.location.merge(self.current_span()?);
            left = Locatable::new(
                location,
                Expression::Binary(bin_op.value, left.map(Box::new), right.map(Box::new)),
            );
        }

        Ok(left)
    }

    fn parse_prefix_unary_expression(&mut self) -> ParseResult<Locatable<Expression>> {
        let token = self.current.as_ref().unwrap();
        if let Ok(un_op) = UnaryOp::try_from(&token.value) {
            self.create_unop(un_op)
        } else if is!(self, current, Token::Symbol(Symbol::OpenParen))
            && is!(self, next, Token::Keyword(kw) if kw.is_for_type())
        {
            self.parse_cast()
        } else if is!(self, current, Token::Symbol(Symbol::Sizeof)) {
            self.parse_sizeof()
        } else {
            self.parse_primary_expression()
        }
    }

    fn create_unop(&mut self, un_op: UnaryOp) -> ParseResult<Locatable<Expression>> {
        let location = self.current_span()?;
        self.advance()?;
        let expr = self.parse_prefix_unary_expression()?;
        let location = location.merge(expr.location);
        let expr = Expression::Unary(un_op, expr.map(Box::new));
        let locatable = Locatable::new(location, expr);
        Ok(locatable)
    }

    fn parse_cast(&mut self) -> ParseResult<Locatable<Expression>> {
        debug_assert!(is!(self, current, Token::Symbol(Symbol::OpenParen)));
        debug_assert!(is!(self, next, Token::Keyword(kw) if kw.is_for_type()));
        let location = self.current_span()?;
        self.advance()?;
        let ty = self.parse_declaration()?;
        confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
        let expr = self.parse_prefix_unary_expression()?;
        let location = location.merge(self.last_span);
        let expr = Expression::Cast(ty, expr.map(Box::new));
        let locatable = Locatable::new(location, expr);
        Ok(locatable)
    }

    fn parse_sizeof(&mut self) -> ParseResult<Locatable<Expression>> {
        debug_assert!(is!(self, current, Token::Symbol(Symbol::Sizeof)));
        let location = self.current_span()?;
        self.advance()?;
        let expr = if is!(self, current, Token::Symbol(Symbol::OpenParen))
            && is!(self, next, Token::Keyword(kw) if kw.is_for_type())
        {
            self.advance()?;
            let ty = self.parse_declaration()?;
            confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
            Expression::Sizeof(ty.map(TypeOrExpression::Type))
        } else {
            Expression::Sizeof(
                self.parse_binary_expression(None)?
                    .map(|expr| TypeOrExpression::Expr(Box::new(expr))),
            )
        };
        let location = location.merge(self.last_span);
        let locatable = Locatable::new(location, expr);
        Ok(locatable)
    }

    fn parse_primary_expression(&mut self) -> ParseResult<Locatable<Expression>> {
        let locatable = self.consume()?;
        let span = locatable.location;
        let expression = match locatable.value {
            Token::Literal(literal) => Ok(Expression::Literal(span.into_locatable(literal))),
            Token::Identifier(ident) => Ok(Expression::Variable(span.into_locatable(ident))),
            Token::Symbol(Symbol::OpenParen) => {
                let expr = self.parse_binary_expression(None)?;
                confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), "\t)")?;
                Ok(Expression::Parenthesized(expr.map(Box::new)))
            }
            _ => {
                self.report_error(CompilerError::ExpectedButFound(
                    "Literal or Expression".to_string(),
                    format!("{:#?}", locatable.value),
                    locatable.location,
                ));
                Err(())
            }
        }?;
        let location = span.merge(self.last_span);
        let expression = Locatable::new(location, expression);
        self.parse_postfix_unary_expression(expression)
    }

    fn parse_postfix_unary_expression(
        &mut self,
        primary_expr: Locatable<Expression>,
    ) -> ParseResult<Locatable<Expression>> {
        if self.current.is_none() {
            return Ok(primary_expr);
        }
        let current = self.current.as_ref().unwrap();
        if let Ok(op) = PostfixOp::try_from(&current.value) {
            self.advance()?;
            let location = primary_expr.location.merge(self.last_span);
            let expr = Locatable::new(
                location,
                Expression::PostFix(op, primary_expr.map(Box::new)),
            );
            self.parse_postfix_unary_expression(expr)
        } else if is!(self, current, Token::Symbol(Symbol::OpenParen))
            && matches!(&primary_expr.value, Expression::Variable(_))
        {
            self.parse_function_call(primary_expr)
        } else if is!(self, current, Token::Symbol(Symbol::OpenSquare)) {
            self.parse_index_access(primary_expr)
        } else if is!(self, current, Token::Symbol(Symbol::Dot)) {
            self.parse_member_access(primary_expr)
        } else if is!(self, current, Token::Symbol(Symbol::Arrow)) {
            self.parse_deref_member_access(primary_expr)
        } else {
            Ok(primary_expr)
        }
    }

    fn parse_function_call(
        &mut self,
        primary_expr: Locatable<Expression>,
    ) -> ParseResult<Locatable<Expression>> {
        debug_assert!(is!(self, current, Token::Symbol(Symbol::OpenParen)));
        debug_assert!(matches!(&primary_expr.value, Expression::Variable(_)));
        let location = primary_expr.location;
        let ident = match primary_expr.value {
            Expression::Variable(var) => var,
            _ => panic!("Nothing else should get to this point."),
        };
        self.advance()?;
        let mut args = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseParen)) {
            let expr = self.parse_binary_expression(None)?;
            args.push(expr);
            if is!(self, current, Token::Symbol(Symbol::Comma)) {
                self.advance()?;
            } else {
                break;
            }
        }
        confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), "\t)")?;
        let location = location.merge(self.last_span);
        let expr = Locatable::new(location, Expression::FunctionCall(ident, args));
        self.parse_postfix_unary_expression(expr)
    }

    fn parse_index_access(
        &mut self,
        primary_expr: Locatable<Expression>,
    ) -> ParseResult<Locatable<Expression>> {
        let location = primary_expr.location;
        self.advance()?;
        let index = self.parse_binary_expression(None)?;
        confirm!(self, consume, Token::Symbol(Symbol::CloseSquare) => (), "]")?;
        let location = primary_expr.location.merge(self.last_span);
        let expr = Expression::Index(primary_expr.map(Box::new), index.map(Box::new));
        let locatable = Locatable::new(location, expr);
        self.parse_postfix_unary_expression(locatable)
    }

    fn parse_member_access(
        &mut self,
        primary_expr: Locatable<Expression>,
    ) -> ParseResult<Locatable<Expression>> {
        let location = primary_expr.location;
        self.advance()?;
        let member = self.confirm_identifier()?;
        let location = primary_expr.location.merge(self.last_span);
        let expr = Expression::Member(primary_expr.map(Box::new), member);
        let locatable = Locatable::new(location, expr);
        self.parse_postfix_unary_expression(locatable)
    }

    fn parse_deref_member_access(
        &mut self,
        primary_expr: Locatable<Expression>,
    ) -> ParseResult<Locatable<Expression>> {
        let location = primary_expr.location;
        self.advance()?;
        let member = self.confirm_identifier()?;
        let location = primary_expr.location.merge(self.last_span);
        let expr = Expression::PointerMember(primary_expr.map(Box::new), member);
        let locatable = Locatable::new(location, expr);
        self.parse_postfix_unary_expression(locatable)
    }
}
