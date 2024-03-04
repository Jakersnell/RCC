use arcstr::ArcStr;
use ast::*;
use macros::*;
use rand::RngCore;
use std::cell::RefCell;
use std::rc::Rc;

use crate::lexer::tokens::Token;
use crate::lexer::tokens::{Keyword, Literal};
use crate::lexer::LexResult;
use crate::util::error::{CompilerError, ErrorReporter};
use crate::util::str_intern::InternedStr;
use crate::util::{Locatable, LocatableToken, Span};

pub mod ast;
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

pub struct Parser {
    lexer: Box<dyn Iterator<Item = LexResult>>,
    reporter: Rc<RefCell<dyn ErrorReporter>>,
    global: Vec<InitDeclaration>,
    current: Option<LocatableToken>,
    next: Option<LocatableToken>,
    last_span: Span,
    current_span: Span,
    primed: bool,
}

impl Parser {
    pub fn new(
        reporter: Rc<RefCell<dyn ErrorReporter>>,
        lexer: Box<dyn Iterator<Item = LexResult>>,
    ) -> Self {
        let mut parser = Parser {
            lexer,
            reporter,
            global: Vec::new(),
            current: None,
            next: None,
            last_span: Span::default(),
            current_span: Span::default(),
            primed: false,
        };
        parser.prime().unwrap();
        parser
    }

    #[inline(always)]
    pub(super) fn report_error(&mut self, error: CompilerError) {
        self.reporter.report_error(error);
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
        let locatable = self.current.take();
        self.advance()?;
        let locatable = locatable.unwrap();
        self.last_span = locatable.location;
        Ok(locatable)
    }

    pub(super) fn confirm_identifier(&mut self) -> ParseResult<Locatable<InternedStr>> {
        confirm!(self, consume, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    pub(super) fn confirm_literal(&mut self) -> ParseResult<Locatable<Literal>> {
        confirm!(self, consume, Token::Literal(literal) => literal,  "<literal>")
    }

    pub(super) fn match_binary_op(&mut self) -> ParseResult<Locatable<BinaryOp>> {
        confirm!(self, borrow, |x| {BinaryOp::try_from(x)}, Ok(op) => op, EXPECTED_BINARY)
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
        self.next = match self.lexer.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(errors)) => {
                return Err(());
            }
            None => None,
        };
        Ok(())
    }
}
