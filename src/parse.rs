use crate::ast::{ASTNode, BinOp, BinaryExpression, Expression, UnOp, UnaryExpression};
use crate::error::CompilerError;
use crate::lex::LexResult;
use crate::tokens::Literal;
use crate::tokens::Symbol;
use crate::tokens::Token;
use crate::util::{CompilerResult, ExpressionNode, Locatable, LocatableToken, Program, Span};
use std::io;
use std::path::PathBuf;

pub struct Parser<L>
where
    L: Iterator<Item = LexResult> + Default + TryFrom<PathBuf, Error = io::Error>,
{
    lexer: L,
    program: Program,
    global: Vec<ASTNode>,
    current: Option<LocatableToken>,
    next: Option<LocatableToken>,
    span: Span,
}

macro_rules! is {
    ($invoker:ident, $current_or_next:ident,  $pattern:pat $(if $guard:expr)? $(,)?) => {
        $invoker.$current_or_next.as_ref().is_some_and(|locatable| matches!(&locatable.value,$pattern $(if $guard)?))
    };
}

macro_rules! confirm {
    ($invoker:ident, $pattern:pat, $expected_str:literal) => {
        let locatable = $invoker.consume()?;
        match locatable.value {
            $pattern => (),
            token => {
                $invoker.span.end = locatable.location.end;
                let string = format!(
                    "Expected one of the following tokens:\n{}\nFound {:#?}\n",
                    $expected_str, token
                );
            }
        };
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
        let body = self.get_body();
        let mut program = self.program;
        program.body = Some(body);
        program
    }

    fn prime(&mut self) -> CompilerResult<()> {
        self.advance()?;
        self.advance()?;
        Ok(())
    }

    fn get_body(&mut self) -> CompilerResult<Vec<ASTNode>> {
        self.prime();

        while self.current.is_some() {
            let global_declaration = self.parse_global()?;
            self.global.push(global_declaration);
        }

        Ok(std::mem::take(&mut self.global))
    }

    #[inline(always)]
    fn unexpected_eof(&self, message: String) -> Vec<Locatable<CompilerError>> {
        vec![Locatable::new(self.span, CompilerError::UnexpectedEOF)]
    }

    fn consume(&mut self) -> CompilerResult<LocatableToken> {
        if self.current.is_none() {
            let mut errors = Vec::new();
            self.get_all_errors(&mut errors);
            return Err(errors);
        }
        let locatable = self.current.take();
        self.advance()?;
        let locatable = locatable.unwrap();
        Ok(locatable)
    }

    /// Advances the lexer without checking for EOF
    fn advance(&mut self) -> CompilerResult<()> {
        self.current = self.next.take();
        self.next = match self.lexer.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(errors)) => {
                let mut errors = errors;
                self.get_all_errors(&mut errors);
                return Err(errors);
            }
            None => None,
        };
        Ok(())
    }

    fn get_all_errors(&mut self, errors: &mut Vec<Locatable<CompilerError>>) {
        let all_results = std::mem::take(&mut self.lexer).collect::<Vec<_>>();
        let all_errors = all_results
            .into_iter()
            .filter_map(|r| if let Err(e) = r { Some(e) } else { None })
            .flatten();
        let mut errors = errors;
        errors.extend(all_errors);
    }

    fn parse_global(&mut self) -> CompilerResult<ASTNode> {
        self.span.start = self.span.end;
        let expr = self.parse_binary_expression(None)?;
        confirm!(self, Token::Symbol(Symbol::Semicolon), ";");
        Ok(ASTNode::Expression(expr))
    }

    fn parse_declaration(&mut self) -> CompilerResult<ASTNode> {
        if is!(self, current, Token::Keyword(x) if x.is_type()) {
            self.advance()?;

            if is!(self, next, Token::Identifier(_)) {
                self.advance()?;
                // could be function or variable declaration
            } else {
                // this is cause for an error.
            }
        } else {
            // this is cause for an error.
        }

        todo!()
    }

    fn parse_binary_expression(&mut self, lp: Option<u8>) -> CompilerResult<ExpressionNode> {
        let lp = lp.unwrap_or(0);
        let mut left = self.parse_unary_expression()?;
        loop {
            if self.current.is_none() {
                return Err(self.unexpected_eof(
                    "Expected one of the following tokens:\n\t+\n\t-\n\t*\n\t/\n\t%\n\t&\n\t|\n\t^\n Found EOF".to_string(),
                ));
            }
            let token = self.current.as_ref().unwrap();
            let bin_op_result = BinOp::try_from(&token.value);
            if bin_op_result.is_err() {
                break;
            }
            let bin_op = bin_op_result.unwrap();
            let rp = bin_op.precedence();
            if rp == 0 || rp < lp {
                break;
            }
            self.span.end = token.location.end;
            self.advance()?;
            let right = self.parse_binary_expression(Some(rp))?;
            left = Locatable::new(
                self.span,
                Expression::Binary(BinaryExpression {
                    left: Box::new(left),
                    op: bin_op,
                    right: Box::new(right),
                }),
            );
        }
        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> CompilerResult<ExpressionNode> {
        if self.current.is_none() {
            return Err(self.unexpected_eof(
                "Expected one of the following tokens:+\n\t-\n\t!\n\t~\n\t*\n\t&\n\tsizeof\n\t<identifier>\n\tliteral\n\t(\n Found EOF".to_string(),
            ));
        }
        let mut node;
        let token = self.current.as_ref().unwrap();
        let un_op_result = UnOp::try_from(&token.value);

        if un_op_result
            .as_ref()
            .is_ok_and(|un_op| 0 < un_op.precedence())
        {
            let un_op = un_op_result.unwrap();
            self.span.end = token.location.end;
            self.advance()?;
            let expr = self.parse_unary_expression()?;
            node = Ok(Locatable::new(
                self.span,
                Expression::Unary(UnaryExpression {
                    op: un_op,
                    right: Box::new(expr),
                }),
            ));
        } else {
            node = self.parse_primary_expression();
        }

        node
    }

    fn parse_primary_expression(&mut self) -> CompilerResult<ExpressionNode> {
        if is!(self, current, Token::Symbol(Symbol::OpenParen)) {
            self.advance()?;
            let expr = self.parse_binary_expression(None)?;
            confirm!(self, Token::Symbol(Symbol::CloseParen), "\t)");
            return Ok(expr);
        }

        self.parse_literal_or_variable()
    }

    fn parse_literal_or_variable(&mut self) -> CompilerResult<ExpressionNode> {
        let locatable = self.consume()?;
        if let Token::Literal(literal) = locatable.value {
            Ok(Locatable::new(self.span, Expression::Literal(literal)))
        } else if let Token::Identifier(identifier) = locatable.value {
            Ok(Locatable::new(self.span, Expression::Variable(identifier)))
        } else {
            self.span.end = locatable.location.end;
            Err(vec![Locatable::new(
                self.span,
                CompilerError::ExpectedVariety(
                    "int, long, long long, char, float, or double".to_string(),
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
