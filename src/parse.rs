use std::io;
use std::path::PathBuf;

use crate::ast::{
    AssignOp, BinaryOp, DataType, Declaration, DeclarationType, Expression, InitDeclaration,
    Statement, UnaryOp,
};
use crate::error::CompilerError;
use crate::lex::LexResult;
use crate::str_intern::InternedStr;
use crate::tokens::Symbol;
use crate::tokens::Token;
use crate::tokens::{Keyword, Literal};
use crate::util::{CompilerResult, Locatable, LocatableToken, Program, Span};

static EXPECTED_UNARY: &str = "+, -, !, ~, *, &, sizeof";
static EXPECTED_BINARY: &str = "+, -, *, /, %, &, |, ^, <<, >>, <, <=, >, >=, ==, !=, &&, ||";
static EXPECTED_TYPE: &str = "int, long, char, float, double";

pub struct Parser<L>
where
    L: Iterator<Item = LexResult> + Default + TryFrom<PathBuf, Error = io::Error>,
{
    lexer: L,
    program: Program,
    global: Vec<InitDeclaration>,
    current: Option<LocatableToken>,
    next: Option<LocatableToken>,
    span: Span,
}

macro_rules! is {
    ($invoker:ident, $current_or_next:ident,  $pattern:pat $(if $guard:expr)? $(,)?) => {
        $invoker.$current_or_next.as_ref().is_some_and(|locatable| matches!(&locatable.value,$pattern $(if $guard)?))
    };
}

macro_rules! confirm_body {
    (
        $value:ident,
        $location:ident,
        $closure:expr,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {{
        let formatted = format!("{:#?}", $value);
        #[allow(clippy::redundant_closure_call)]
        match $closure($value) {
            $pattern $(if $guard)? => Ok(Locatable::new($location, $if_ok)),
            _ => Err(vec![Locatable{
                location:$location,
                value:CompilerError::ExpectedVariety($if_err.to_string(), formatted)
            }])
        }
    }};
}

macro_rules! confirm {

    (
        $invoker:ident,
        consume,
        $closure:expr,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {{
        let locatable = $invoker.consume()?;
        let location = locatable.location;
        let value = locatable.value;
        confirm_body!(value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
    }};

    (
        $invoker:ident,
        consume,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {
        confirm!( $invoker, consume, |x| {x}, $pattern $(if $guard)? => $if_ok, $if_err)
    };

    (
        $invoker:ident,
        consume,
        $pattern:pat $(if $guard:expr)?,
        $if_err:literal
    ) => {
        confirm!( $invoker, consume, |x| {x}, $pattern $(if $guard)? => (), $if_err)
    };

    (
        $invoker:ident,
        borrow,
        $closure:expr,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {{
        let locatable = &$invoker.current.as_ref().unwrap();
        let value = &locatable.value;
        let location = locatable.location;
        confirm_body!(value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
    }};

        (
        $invoker:ident,
        borrow,
        $pattern:pat $(if $guard:expr)? => $if_ok:expr,
        $if_err:literal
    ) => {
        confirm!( $invoker, borrow, |x| {x}, $pattern $(if $guard)? => $if_ok, $if_err)
    };

    (
        $invoker:ident,
        borrow,
        $pattern:pat $(if $guard:expr)?,
        $if_err:literal
    ) => {
        confirm!( $invoker, borrow, |x| {x}, $pattern $(if $guard)? => (), $if_err)
    };
}

impl<L> Parser<L>
where
    L: Iterator<Item = LexResult> + Default + TryFrom<PathBuf, Error = io::Error>,
{
    pub fn new(program: Program) -> io::Result<Self> {
        let lexer = L::try_from(PathBuf::from(&program.file_name))?;
        Ok(Self::from_lexer(program, lexer))
    }

    pub fn from_lexer(program: Program, lexer: L) -> Self {
        Self {
            lexer,
            program,
            global: Vec::new(),
            current: None,
            next: None,
            span: Span::new(0, 0),
        }
    }

    pub fn parse(mut self) -> Program {
        let body = self.get_body().map_err(|mut errors| {
            self.get_all_errors(&mut errors);
            errors
        });
        let mut program = self.program;
        program.body = Some(body);
        program
    }

    fn get_body(&mut self) -> CompilerResult<Vec<InitDeclaration>> {
        self.prime();

        while self.current.is_some() {
            let global_declaration = self.parse_init_declaration()?;
            self.global.push(global_declaration);
        }

        Ok(std::mem::take(&mut self.global))
    }

    #[inline(always)]
    fn prime(&mut self) -> CompilerResult<()> {
        for _ in 0..2 {
            self.advance()?;
        }
        Ok(())
    }

    #[inline(always)]
    fn check_for_eof(&mut self, expected: &'static str) -> CompilerResult<()> {
        if self.current.is_none() {
            return Err(vec![Locatable::new(
                self.span,
                CompilerError::UnexpectedEOF,
            )]);
        }
        Ok(())
    }

    fn consume(&mut self) -> CompilerResult<LocatableToken> {
        self.check_for_eof("token")?;
        let locatable = self.current.take();
        self.advance()?;
        let locatable = locatable.unwrap();
        Ok(locatable)
    }

    #[inline(always)]
    fn confirm_identifier(&mut self) -> CompilerResult<Locatable<InternedStr>> {
        confirm!(self, consume, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    #[inline(always)]
    fn confirm_literal(&mut self) -> CompilerResult<Locatable<Literal>> {
        confirm!(self, consume, Token::Literal(literal) => literal,  "<literal>")
    }

    #[inline(always)]
    fn match_binary_op(&mut self) -> CompilerResult<Locatable<BinaryOp>> {
        confirm!(self, borrow, |x| {BinaryOp::try_from(x)}, Ok(op) => op, "+, -, *, /, %, &, |, ^, <<, >>, <, <=, >, >=, ==, !=, &&, ||")
    }

    #[inline(always)]
    fn confirm_unary_op(&mut self) -> CompilerResult<Locatable<UnaryOp>> {
        confirm!(self, consume, |x| {UnaryOp::try_from(&x)}, Ok(op) => op, "+, -, !, ~, *, &, sizeof")
    }

    #[inline(always)]
    fn confirm_type(&mut self) -> CompilerResult<Locatable<DataType>> {
        confirm!(self, consume, |x| {DataType::try_from(&x)}, Ok(x) => x, "int, long, char, float, double")
    }

    #[inline(always)]
    fn match_identifier(&mut self) -> CompilerResult<Locatable<InternedStr>> {
        confirm!(self, borrow, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    #[inline(always)]
    fn skip_empty_statements(&mut self) -> CompilerResult<()> {
        while is!(self, current, Token::Symbol(Symbol::Semicolon)) {
            self.advance()?;
        }
        Ok(())
    }

    /// Advances the lexer without checking for EOF
    fn advance(&mut self) -> CompilerResult<()> {
        self.current = self.next.take();
        self.next = match self.lexer.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(errors)) => {
                return Err(errors);
            }
            None => None,
        };
        Ok(())
    }

    // Collects all errors from the lexer and appends them to the errors vector
    fn get_all_errors(&mut self, errors: &mut Vec<Locatable<CompilerError>>) {
        let all_results = std::mem::take(&mut self.lexer).collect::<Vec<_>>();
        let all_errors = all_results
            .into_iter()
            .filter_map(|r| if let Err(e) = r { Some(e) } else { None })
            .flatten();
        let mut errors = errors;
        errors.extend(all_errors);
    }

    fn parse_init_declaration(&mut self) -> CompilerResult<InitDeclaration> {
        self.skip_empty_statements()?;
        let keyword = self.confirm_type()?;
        let identifier = self.match_identifier()?;

        // currently we only have unit types so this is hardcoded as DC::Type
        let dec_type = DeclarationType::Type {
            specifiers: None,
            ty: keyword.value,
        };

        let declaration = Declaration {
            ty: dec_type,
            name: Some(identifier.value),
        };

        let init_declaration = match &self.next.as_ref().unwrap().value {
            Token::Symbol(Symbol::Semicolon) => {
                self.advance()?;
                InitDeclaration::Declaration(declaration, None)
            }
            symbol if AssignOp::try_from(symbol).is_ok() => {
                let expr = self.parse_binary_expression(None)?;
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                InitDeclaration::Declaration(declaration, Some(expr))
            }
            Token::Symbol(Symbol::OpenParen) => {
                todo!("parse function declaration")
            }
            _ => Err(vec![Locatable::new(
                self.span,
                CompilerError::ExpectedButFound(
                    "function or variable declaration".to_string(),
                    format!("{:#?}", self.current.as_ref().unwrap().value),
                ),
            )])?,
        };

        Ok(init_declaration)
    }
    fn parse_binary_expression(&mut self, lp: Option<u8>) -> CompilerResult<Expression> {
        let lp = lp.unwrap_or(0);
        let mut left = self.parse_unary_expression()?;
        loop {
            if is!(self, current, Token::Symbol(Symbol::Semicolon)) {
                break;
            }
            let bin_op = self.match_binary_op();
            if bin_op.is_err() {
                break;
            }
            let bin_op = bin_op.unwrap();
            let rp = bin_op.value.precedence();
            if rp == 0 || rp <= lp {
                break;
            }
            self.advance()?;
            let right = self.parse_binary_expression(Some(rp))?;
            left = Expression::Binary(bin_op.value, Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> CompilerResult<Expression> {
        let mut node;
        let token = self.current.as_ref().unwrap();

        if let Ok(un_op) = UnaryOp::try_from(&token.value) {
            self.span.end = token.location.end;
            self.advance()?;
            let expr = self.parse_unary_expression()?;
            node = Ok(Expression::Unary(un_op, Box::new(expr)));
        } else {
            node = self.parse_primary_expression();
        }

        node
    }

    fn parse_primary_expression(&mut self) -> CompilerResult<Expression> {
        if is!(self, current, Token::Symbol(Symbol::OpenParen)) {
            self.advance()?;
            let expr = self.parse_binary_expression(None)?;
            confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), "\t)")?;
            return Ok(expr);
        }
        self.parse_literal_or_variable()
    }

    fn parse_literal_or_variable(&mut self) -> CompilerResult<Expression> {
        let locatable = self.consume()?;
        if let Token::Literal(literal) = locatable.value {
            Ok(Expression::Literal(literal))
        } else if let Token::Identifier(identifier) = locatable.value {
            Ok(Expression::Variable(identifier))
        } else {
            self.span.end = locatable.location.end;
            Err(vec![Locatable::new(
                self.span,
                CompilerError::ExpectedButFound(
                    "Literal or Expression".to_string(),
                    format!("{:#?}", locatable.value),
                ),
            )])
        }
    }
}

#[cfg(test)]
macro_rules! make_token {
    ($tk:expr) => {
        Ok(Locatable::new(Span::new(0, 0), $tk))
    };
}

#[test]
fn test_advance_advances_tokens_correctly() {
    let program = Program::new("test".to_string());
    let source = "1 * 1".to_string();
    let lexer = crate::lex::Lexer::new(source);
    let mut parser = Parser::from_lexer(program, lexer);
    parser.prime();
    assert!(parser.consume().is_ok());
    assert!(parser.consume().is_ok());
    assert!(parser.consume().is_ok());
    assert!(parser.consume().is_err());
}
