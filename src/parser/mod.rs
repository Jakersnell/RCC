use rand::RngCore;

use macros::*;

use crate::data::ast::*;
use crate::data::tokens::{Keyword, Literal};
use crate::data::tokens::{Symbol, Token};
use crate::util::{Locatable, LocatableToken, Span};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;

pub(super) mod declarations;
pub(super) mod expressions;
pub(super) mod macros;
pub(super) mod statements;

pub(super) static EXPECTED_UNARY: &str = "+, -, !, ~, *, &, sizeof, ++, --";
pub(super) static EXPECTED_BINARY: &str =
    "+, -, *, /, %, &, |, ^, <<, >>, <, <=, >, >=, ==, !=, &&, ||";
pub(super) static EXPECTED_ASSIGN: &str = "=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=";
pub(super) static EXPECTED_TYPE: &str = "int, long, char, float, double";

pub type ParseResult<T> = Result<T, ()>;

/// Core implementations only here, more is implemented in the supporting files in this directory
pub struct Parser<L>
where
    L: Iterator<Item = Locatable<Token>>,
{
    tokens: L,
    errors: Vec<CompilerError>,
    global: Vec<InitDeclaration>,
    current: Option<LocatableToken>,
    next: Option<LocatableToken>,
    last_span: Span,
    current_span: Span,
}

impl<L> Parser<L>
where
    L: Iterator<Item = Locatable<Token>>,
{
    pub fn new(lexer: L) -> Self {
        Self {
            tokens: lexer,
            errors: Vec::new(),
            global: Vec::new(),
            current: None,
            next: None,
            last_span: Span::default(),
            current_span: Span::default(),
        }
    }

    pub fn parse_all(mut self) -> Result<AbstractSyntaxTree, Vec<CompilerError>> {
        let primer = self.prime();
        if primer.is_err() {
            return Err(self.errors);
        }
        let mut global = Vec::new();
        while self.current.is_some() {
            let init_dec = self.parse_init_declaration();
            if init_dec.is_err() {
                return Err(self.errors);
            }
            global.push(init_dec.unwrap());
        }
        let ast = AbstractSyntaxTree::new(global.into_iter().map(|dec| dec.value).collect());
        Ok(ast)
    }

    #[inline(always)]
    pub(super) fn report_error(&mut self, error: CompilerError) -> ParseResult<()> {
        self.errors.push(error);
        Err(())
    }

    #[inline(always)]
    pub(super) fn check_for_eof(&mut self, expected: &'static str) -> ParseResult<()> {
        if self.current.is_none() {
            self.report_error(CompilerError::UnexpectedEOF);
            Err(())
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    fn prime(&mut self) -> ParseResult<()> {
        self.advance()?;
        self.advance()?;
        Ok(())
    }

    #[inline(always)]
    fn consume(&mut self) -> ParseResult<LocatableToken> {
        self.check_for_eof("token")?;
        let locatable = self.current.take().expect("This should never EOF");
        self.advance()?;
        self.last_span = locatable.location;
        Ok(locatable)
    }

    pub(super) fn confirm_identifier(&mut self) -> ParseResult<Locatable<InternedStr>> {
        confirm!(self, consume, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    pub(super) fn confirm_literal(&mut self) -> ParseResult<Locatable<Literal>> {
        confirm!(self, consume, Token::Literal(literal) => literal,  "<literal>")
    }

    pub(super) fn confirm_semicolon(&mut self) -> ParseResult<Locatable<()>> {
        confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";")
    }

    pub(super) fn confirm_unary_op(&mut self) -> ParseResult<Locatable<UnaryOp>> {
        confirm!(self, consume, |x| {UnaryOp::try_from(&x)}, Ok(op) => op, EXPECTED_UNARY)
    }

    pub(super) fn confirm_type(&mut self) -> ParseResult<Locatable<TypeSpecifier>> {
        confirm!(self, consume, |x| {TypeSpecifier::try_from(&x)}, Ok(x) => x, EXPECTED_TYPE)
    }

    pub(super) fn match_identifier(&mut self) -> ParseResult<Locatable<InternedStr>> {
        confirm!(self, borrow, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    pub(super) fn match_keyword(&mut self) -> ParseResult<Locatable<Keyword>> {
        confirm!(self, borrow, Token::Keyword(keyword) => *keyword, "<keyword>")
    }

    pub(super) fn match_assign(&mut self) -> ParseResult<Locatable<AssignOp>> {
        confirm!(self, borrow, |x| {AssignOp::try_from(x)}, Ok(op) => op, EXPECTED_ASSIGN)
    }

    pub(super) fn match_binary_op(&mut self) -> ParseResult<Locatable<BinaryOp>> {
        confirm!(self, borrow, |x| {BinaryOp::try_from(x)}, Ok(op) => op, EXPECTED_BINARY)
    }

    pub(super) fn current_span(&mut self) -> ParseResult<Span> {
        match self.current.as_ref() {
            Some(locatable) => Ok(locatable.location),
            None => {
                self.report_error(CompilerError::UnexpectedEOF);
                Err(())
            }
        }
    }

    pub(super) fn advance(&mut self) -> ParseResult<()> {
        self.current = self.next.take();
        self.next = self.tokens.next();
        Ok(())
    }
}
