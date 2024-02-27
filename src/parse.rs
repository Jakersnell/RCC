use std::io;
use std::path::PathBuf;

use crate::ast::{
    AssignOp, BinaryOp, Block, Declaration, DeclarationSpecifier, Declarator, Expression,
    FunctionDeclaration, InitDeclaration, PostfixOp, Statement, StorageSpecifier, TypeOrExpression,
    TypeQualifier, TypeSpecifier, UnaryOp, VariableDeclaration,
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

macro_rules! match_token {
    ($invoker:ident, $current_or_next:ident, $closure:expr, $pattern:pat $(if $guard:expr)? => $if_ok:expr) => {
        $invoker.$current_or_next.as_ref().map(|next|{
            #[allow(clippy::redundant_closure_call)]
            match $closure(&next.value) {
                $pattern $(if $guard)? => Some(Locatable{
                    location: next.location,
                    value: $if_ok
                }),
                _ => None
            }
        }).flatten()
    };
    ($invoker:ident, $current_or_next:ident, $pattern:pat $(if $guard:expr)? => $if_ok:expr) => {
        match_token!($invoker, $current_or_next, |x|{x}, $pattern $(if $guard)? => $if_ok)
    };
    ($invoker:ident, $current_or_next:ident, $closure:expr, $pattern:pat $(if $guard:expr)?) => {
        match_token!($invoker, $current_or_next, $closure, $pattern $(if $guard)? => Some($if_ok))
    };
    ($invoker:ident, $current_or_next:ident, $pattern:pat $(if $guard:expr)?) => {
        match_token!($invoker, $current_or_next, $pattern $(if $guard)? => Some(()))
    };

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
        confirm!(value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
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
        confirm!(value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
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

    /// this seemed like the only way to avoid returning a result in the constructor
    #[inline(always)]
    fn prime(&mut self) -> CompilerResult<()> {
        self.advance()?;
        self.advance()?;
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
    fn confirm_type(&mut self) -> CompilerResult<Locatable<TypeSpecifier>> {
        confirm!(self, consume, |x| {TypeSpecifier::try_from(&x)}, Ok(x) => x, "int, long, char, float, double")
    }

    #[inline(always)]
    fn match_identifier(&mut self) -> CompilerResult<Locatable<InternedStr>> {
        confirm!(self, borrow, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    #[inline(always)]
    fn match_keyword(&mut self) -> CompilerResult<Locatable<Keyword>> {
        confirm!(self, borrow, Token::Keyword(keyword) => *keyword, "<keyword>")
    }

    #[inline(always)]
    fn match_assign(&mut self) -> CompilerResult<Locatable<AssignOp>> {
        confirm!(self, borrow, |x| {AssignOp::try_from(x)}, Ok(op) => op, "=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=")
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

    fn get_body(&mut self) -> CompilerResult<Vec<InitDeclaration>> {
        self.prime();

        while self.current.is_some() {
            let global_declaration = self.parse_init_declaration()?;
            self.global.push(global_declaration);
        }

        Ok(std::mem::take(&mut self.global))
    }

    fn parse_init_declaration(&mut self) -> CompilerResult<InitDeclaration> {
        self.skip_empty_statements()?;
        let dec = self.parse_declaration()?;

        if is!(
            self,
            current,
            Token::Symbol(Symbol::Semicolon) | Token::Symbol(Symbol::Comma)
        ) || is!(self, current, token if token.is_assign_op() )
        {
            let variable_declaration = self.parse_variable_declaration(dec)?;
            confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";")?;
            Ok(InitDeclaration::Declaration(variable_declaration))
        } else if is!(self, current, Token::Symbol(Symbol::OpenParen)) {
            let function = self.parse_function_declaration(dec)?;
            Ok(InitDeclaration::Function(function))
        } else {
            Err(vec![Locatable::new(
                self.span,
                CompilerError::ExpectedButFound(
                    "function or variable declaration".to_string(),
                    format!("{:#?}", self.current.as_ref().unwrap().value),
                ),
            )])?
        }
    }

    fn parse_declaration(&mut self) -> CompilerResult<Declaration> {
        let specifier = self.parse_declaration_specifier()?;
        let declarator = self.parse_declarator()?;
        Ok(Declaration {
            specifier,
            declarator,
        })
    }

    fn parse_declarator(&mut self) -> CompilerResult<Declarator> {
        // I apologize in advance for the following code lol
        if is!(self, current, Token::Symbol(Symbol::Star)) {
            self.advance()?;
            let to = Box::new(self.parse_declarator()?);
            Ok(Declarator::Pointer { to })
        } else if is!(self, current, Token::Symbol(Symbol::OpenSquare)) {
            self.advance()?;
            let of = Box::new(self.parse_declarator()?);
            let size = if let Some(Locatable {
                location,
                value: (integer, suffix),
            }) = match_token!(self, current, Token::Literal(Literal::Integer {value, suffix}) => (*value, suffix.clone()))
            {
                if suffix.is_some() {
                    return Err(vec![Locatable::new(
                        location,
                        CompilerError::CustomError(
                            "Suffixes in array sizes are not currently supported.".to_string(),
                        ),
                    )]);
                }
                self.advance()?;
                Some(integer as usize)
            } else {
                None
            };
            confirm!(self, consume, Token::Symbol(Symbol::CloseSquare) => (), "]")?;
            Ok(Declarator::Array { of, size })
        } else if let Ok(ident) = self.match_identifier() {
            self.advance()?;
            Ok(Declarator::Unit {
                ident: ident.clone(),
            })
        } else {
            Ok(Declarator::None)
        }
    }

    fn parse_declaration_specifier(&mut self) -> CompilerResult<DeclarationSpecifier> {
        let mut storage_specifiers = Vec::new();
        while let Some(storage_specifier) =
            match_token!(self, current, |x|{StorageSpecifier::try_from(x)}, Ok(x) => x)
        {
            storage_specifiers.push(storage_specifier.value);
            self.advance()?;
        }
        let mut type_qualifiers = Vec::new();
        while let Some(type_qualifier) =
            match_token!(self, current, |x|{TypeQualifier::try_from(x)}, Ok(x) => x)
        {
            type_qualifiers.push(type_qualifier.value);
            self.advance()?;
        }
        let mut type_specifiers = Vec::new();
        while let Some(type_specifier) =
            match_token!(self, current, |x|{TypeSpecifier::try_from(x)}, Ok(x) => x)
        {
            type_specifiers.push(type_specifier.value);
            self.advance()?;
        }
        Ok(DeclarationSpecifier {
            specifiers: storage_specifiers,
            qualifiers: type_qualifiers,
            ty: type_specifiers,
        })
    }

    fn parse_function_declaration(
        &mut self,
        declaration: Declaration,
    ) -> CompilerResult<FunctionDeclaration> {
        confirm!(self, consume, Token::Symbol(Symbol::OpenParen) => (), "(")?;
        let mut parameters = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseParen)) {
            let param = self.parse_declaration()?;
            parameters.push(param);
            if is!(self, current, Token::Symbol(Symbol::Comma)) {
                self.advance()?;
            } else {
                break;
            }
        }
        // note to self: parse varargs here
        confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
        let body = if is!(self, current, Token::Symbol(Symbol::OpenCurly)) {
            Some(self.parse_compound_statement()?)
        } else {
            confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
            None
        };

        Ok(FunctionDeclaration {
            declaration,
            parameters,
            varargs: false,
            body,
        })
    }

    fn parse_variable_declaration(
        &mut self,
        declaration: Declaration,
    ) -> CompilerResult<VariableDeclaration> {
        // debug_assert!(declaration.name.is_some());
        let initializer = if is!(self, current, Token::Symbol(Symbol::Equal)) {
            self.advance()?;
            Some(self.parse_binary_expression(None)?)
        } else {
            None
        };

        Ok(VariableDeclaration {
            declaration,
            initializer,
        })
    }

    fn parse_compound_statement(&mut self) -> CompilerResult<Block> {
        confirm!(self, consume, Token::Symbol(Symbol::OpenCurly) => (), "{")?;
        let mut body = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseCurly)) {
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }
        confirm!(self, consume, Token::Symbol(Symbol::CloseCurly) => (), "}")?;
        Ok(Block(body))
    }

    fn parse_statement(&mut self) -> CompilerResult<Statement> {
        self.check_for_eof("statement")?;
        match self.current.as_ref().unwrap().value {
            Token::Symbol(Symbol::OpenCurly) => {
                let block = self.parse_compound_statement()?;
                Ok(Statement::Block(block))
            }
            Token::Keyword(keyword) if keyword.is_type() => {
                let dec = self.parse_declaration()?;
                let variable_declaration = self.parse_variable_declaration(dec)?;
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                Ok(Statement::Declaration(variable_declaration))
            }
            Token::Keyword(Keyword::Return) => {
                self.advance()?;
                if is!(self, current, Token::Symbol(Symbol::Semicolon)) {
                    Ok(Statement::Return(None))
                } else {
                    let expr = self.parse_binary_expression(None)?;
                    confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                    Ok(Statement::Return(Some(expr)))
                }
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
                Ok(Statement::If(condition, stmt, else_stmt))
            }
            Token::Keyword(Keyword::While) => {
                self.advance()?;
                confirm!(self, consume, Token::Symbol(Symbol::OpenParen) => (), "(")?;
                let condition = self.parse_binary_expression(None)?;
                confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), ")")?;
                let stmt = Box::new(self.parse_statement()?);
                Ok(Statement::While(condition, stmt))
            }
            Token::Keyword(Keyword::Continue) => Ok(Statement::Continue),
            Token::Keyword(Keyword::Break) => Ok(Statement::Break),
            _ => {
                let stmt = self
                    .parse_assignment_expression()
                    .map(Statement::Expression);
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
                stmt
            }
        }
    }

    fn parse_assignment_expression(&mut self) -> CompilerResult<Expression> {
        let mut root: Option<Expression> = None;
        while let (Ok(ident), Some(op)) = (
            self.match_identifier(),
            match_token!(self, next, |x|{AssignOp::try_from(x)}, Ok(x) => x),
        ) {
            self.advance()?;
            self.advance()?;
            let right = self.parse_assignment_expression()?;
            root = Some(Expression::Assignment(
                op.value,
                ident.value,
                Box::new(right),
            ));
        }
        if let Some(root) = root {
            Ok(root)
        } else {
            self.parse_binary_expression(None)
        }
    }

    fn parse_binary_expression(&mut self, lp: Option<u8>) -> CompilerResult<Expression> {
        let lp = lp.unwrap_or(0);
        let mut left = self.parse_prefix_unary_expression()?;

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

    fn parse_prefix_unary_expression(&mut self) -> CompilerResult<Expression> {
        let mut node;
        let token = self.current.as_ref().unwrap();

        if let Ok(un_op) = UnaryOp::try_from(&token.value) {
            self.span.end = token.location.end;
            self.advance()?;
            let expr = self.parse_prefix_unary_expression()?;
            node = Ok(Expression::Unary(un_op, Box::new(expr)));
        } else if is!(self, current, Token::Symbol(Symbol::OpenParen)) {
            self.advance()?;
            let ty = self.parse_declaration_specifier()?;
            todo!()
        } else if is!(self, current, Token::Symbol(Symbol::Sizeof)) {
            self.advance()?;
            node = self.parse_sizeof();
        } else {
            node = self.parse_primary_expression();
        }

        node
    }

    fn parse_sizeof(&mut self) -> CompilerResult<Expression> {
        if is!(self, current, Token::Symbol(Symbol::OpenParen))
            && is!(self, next, Token::Keyword(kw) if kw.is_type())
        {
            let ty = self.parse_declaration_specifier()?;
            self.advance()?;
            self.advance()?;
            Ok(Expression::Sizeof(TypeOrExpression::Type(ty)))
        } else {
            Ok(Expression::Sizeof(TypeOrExpression::Expr(Box::new(
                self.parse_binary_expression(None)?,
            ))))
        }
    }

    fn parse_primary_expression(&mut self) -> CompilerResult<Expression> {
        let locatable = self.consume()?;
        let expression = if Token::Symbol(Symbol::OpenParen) == locatable.value {
            let expr = self.parse_binary_expression(None)?;
            confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), "\t)")?;
            Ok(expr)
        } else if let Token::Literal(literal) = locatable.value {
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
        }?;

        self.parse_postfix_unary_expression(expression)
    }

    fn parse_postfix_unary_expression(
        &mut self,
        primary_expr: Expression,
    ) -> CompilerResult<Expression> {
        if self.current.is_none() {
            return Ok(primary_expr);
        }
        let current = self.current.as_ref().unwrap();
        if let Ok(op) = PostfixOp::try_from(&current.value) {
            self.parse_postfix_unary_expression(Expression::PostFix(op, Box::new(primary_expr)))
        } else if is!(self, current, Token::Symbol(Symbol::OpenParen))
            && matches!(&primary_expr, Expression::Variable(_))
        {
            let ident = match primary_expr {
                Expression::Variable(var) => var,
                _ => panic!("Nothing else should get to this point."),
            };
            self.advance()?;
            let mut args = Vec::new();
            while !is!(self, current, Token::Symbol(Symbol::CloseParen)) {
                self.advance()?;
                let expr = self.parse_binary_expression(None)?;
                args.push(expr);
                if is!(self, current, Token::Symbol(Symbol::Comma)) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            confirm!(self, consume, Token::Symbol(Symbol::CloseParen) => (), "\t)")?;
            self.parse_postfix_unary_expression(Expression::FunctionCall(ident, args))
        } else if is!(self, current, Token::Symbol(Symbol::OpenSquare)) {
            self.advance()?;
            let index = self.parse_binary_expression(None)?;
            confirm!(self, consume, Token::Symbol(Symbol::CloseSquare) => (), "]")?;
            self.parse_postfix_unary_expression(Expression::Index(
                Box::new(primary_expr),
                Box::new(index),
            ))
        } else if is!(self, current, Token::Symbol(Symbol::Dot)) {
            self.advance()?;
            let member = self.confirm_identifier()?;
            self.parse_postfix_unary_expression(Expression::Member(
                Box::new(primary_expr),
                member.value,
            ))
        } else if is!(self, current, Token::Symbol(Symbol::Arrow)) {
            self.advance()?;
            let member = self.confirm_identifier()?;
            self.parse_postfix_unary_expression(Expression::PointerMember(
                Box::new(primary_expr),
                member.value,
            ))
        } else {
            Ok(primary_expr)
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
