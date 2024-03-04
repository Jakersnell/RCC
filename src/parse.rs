use arcstr::ArcStr;

use crate::ast::{
    AssignOp, BinaryOp, Block, Declaration, DeclarationSpecifier, DeclaratorType, Expression,
    FunctionDeclaration, InitDeclaration, PostfixOp, Statement, StorageSpecifier,
    StructDeclaration, TypeOrExpression, TypeQualifier, TypeSpecifier, UnaryOp,
    VariableDeclaration,
};
use crate::error::{CompilerError, ErrorReporter};
use crate::lex::LexResult;
use crate::str_intern::InternedStr;
use crate::tokens::Symbol;
use crate::tokens::Token;
use crate::tokens::{Keyword, Literal};
use crate::util::{Locatable, LocatableToken, Program, Span};

static EXPECTED_UNARY: &str = "+, -, !, ~, *, &, sizeof";
static EXPECTED_BINARY: &str = "+, -, *, /, %, &, |, ^, <<, >>, <, <=, >, >=, ==, !=, &&, ||";
static EXPECTED_TYPE: &str = "int, long, char, float, double";

pub type ParseResult<T> = Result<T, ()>;

pub trait AstParser<'a, L: Iterator<Item = LexResult> + From<ArcStr>, E: ErrorReporter + 'a>:
    Iterator<Item = Locatable<InitDeclaration>> + From<&'a mut Program<E>>
where
    L: Iterator<Item = LexResult> + From<ArcStr>,
    E: ErrorReporter,
{
}
impl<'a, L, E> Iterator for Parser<'a, L, E>
where
    L: From<ArcStr> + Iterator<Item = LexResult>,
    E: ErrorReporter,
{
    type Item = Locatable<InitDeclaration>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

impl<'a, L, E> From<&'a mut Program<E>> for Parser<'a, L, E>
where
    L: From<ArcStr> + Iterator<Item = LexResult>,
    E: ErrorReporter,
{
    fn from(value: &'a mut Program<E>) -> Self {
        todo!()
    }
}

impl<'a, L, E> AstParser<'a, L, E> for Parser<'a, L, E>
where
    L: Iterator<Item = LexResult> + From<ArcStr>,
    E: ErrorReporter,
{
}

macro_rules! is {
    ($invoker:ident, $current_or_next:ident,  $pattern:pat $(if $guard:expr)? $(,)?) => {
        $invoker.$current_or_next.as_ref().is_some_and(|locatable| matches!(&locatable.value, $pattern $(if $guard)?))
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
        confirm!($invoker, value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
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
        confirm!($invoker, value, location, $closure, $pattern $(if $guard)? => $if_ok, $if_err)
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
        $invoker:ident,
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
            _ => {
                $invoker.report_error(CompilerError::ExpectedVariety($if_err.to_string(), formatted, $location));
                Err(())
            }
        }
    }};
}

pub struct Parser<'a, L, E>
where
    L: Iterator<Item = LexResult> + From<ArcStr>,
    E: ErrorReporter,
{
    lexer: L,
    reporter: &'a mut E,
    global: Vec<InitDeclaration>,
    current: Option<LocatableToken>,
    next: Option<LocatableToken>,
    last_span: Span,
    current_span: Span,
    primed: bool,
}

impl<'a, L, E> Parser<'a, L, E>
where
    L: Iterator<Item = LexResult> + From<ArcStr>,
    E: ErrorReporter,
{
    /// this seemed like the best way to avoid returning a result in the constructor
    #[inline(always)]
    fn prime(&mut self) -> ParseResult<()> {
        self.advance()?;
        self.advance()?;
        Ok(())
    }

    #[inline(always)]
    fn report_error(&mut self, error: CompilerError) {
        self.reporter.report_error(error);
    }

    #[inline(always)]
    fn check_for_eof(&mut self, expected: &'static str) -> ParseResult<()> {
        if self.current.is_none() {
            self.report_error(CompilerError::UnexpectedEOF);
            Err(())
        } else {
            Ok(())
        }
    }

    fn consume(&mut self) -> ParseResult<LocatableToken> {
        self.check_for_eof("token")?;
        let locatable = self.current.take();
        self.advance()?;
        let locatable = locatable.unwrap();
        self.last_span = locatable.location;
        Ok(locatable)
    }

    #[inline(always)]
    fn confirm_identifier(&mut self) -> ParseResult<Locatable<InternedStr>> {
        confirm!(self, consume, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    #[inline(always)]
    fn confirm_literal(&mut self) -> ParseResult<Locatable<Literal>> {
        confirm!(self, consume, Token::Literal(literal) => literal,  "<literal>")
    }

    #[inline(always)]
    fn match_binary_op(&mut self) -> ParseResult<Locatable<BinaryOp>> {
        confirm!(self, borrow, |x| {BinaryOp::try_from(x)}, Ok(op) => op, "+, -, *, /, %, &, |, ^, <<, >>, <, <=, >, >=, ==, !=, &&, ||")
    }

    #[inline(always)]
    fn confirm_unary_op(&mut self) -> ParseResult<Locatable<UnaryOp>> {
        confirm!(self, consume, |x| {UnaryOp::try_from(&x)}, Ok(op) => op, "+, -, !, ~, *, &, sizeof")
    }

    #[inline(always)]
    fn confirm_type(&mut self) -> ParseResult<Locatable<TypeSpecifier>> {
        confirm!(self, consume, |x| {TypeSpecifier::try_from(&x)}, Ok(x) => x, "int, long, char, float, double")
    }

    #[inline(always)]
    fn match_identifier(&mut self) -> ParseResult<Locatable<InternedStr>> {
        confirm!(self, borrow, Token::Identifier(arc_str) => arc_str.clone(), "<identifier>")
    }

    #[inline(always)]
    fn match_keyword(&mut self) -> ParseResult<Locatable<Keyword>> {
        confirm!(self, borrow, Token::Keyword(keyword) => *keyword, "<keyword>")
    }

    #[inline(always)]
    fn match_assign(&mut self) -> ParseResult<Locatable<AssignOp>> {
        confirm!(self, borrow, |x| {AssignOp::try_from(x)}, Ok(op) => op, "=, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=")
    }

    #[inline(always)]
    fn skip_empty_statements(&mut self) -> ParseResult<()> {
        while is!(self, current, Token::Symbol(Symbol::Semicolon)) {
            self.advance()?;
        }
        Ok(())
    }

    #[inline(always)]
    fn current_span(&mut self) -> ParseResult<Span> {
        match self.current.as_ref() {
            Some(locatable) => Ok(locatable.location),
            None => {
                self.report_error(CompilerError::UnexpectedEOF);
                Err(())
            }
        }
    }

    #[inline(always)]
    fn advance(&mut self) -> ParseResult<()> {
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

impl<'a, L, E> Parser<'a, L, E>
where
    L: Iterator<Item = LexResult> + From<ArcStr>,
    E: ErrorReporter,
{
    pub fn parse_init_declaration(&mut self) -> ParseResult<InitDeclaration> {
        let location = self.current_span()?;
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
        } else if is!(self, current, Token::Symbol(Symbol::OpenCurly)) {
            let _struct = self.parse_struct_declaration(dec)?;
            Ok(InitDeclaration::Struct(_struct))
        } else {
            self.report_error(CompilerError::ExpectedButFound(
                "function or variable declaration".to_string(),
                format!("{:#?}", self.current.as_ref().unwrap().value),
                location.merge(self.current_span),
            ));
            Err(())
        }
    }

    pub fn parse_struct_declaration(
        &mut self,
        declaration: Locatable<Declaration>,
    ) -> ParseResult<Locatable<StructDeclaration>> {
        confirm!(self, consume, Token::Symbol(Symbol::OpenCurly), "{")?;
        let mut members = Vec::new();
        while !is!(self, current, Token::Symbol(Symbol::CloseCurly)) {
            let member = self.parse_declaration()?;
            members.push(member);
            confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";");
        }
        confirm!(self, consume, Token::Symbol(Symbol::CloseCurly), "}")?;
        confirm!(self, consume, Token::Symbol(Symbol::Semicolon), ";")?;
        let location = declaration.location.merge(self.current_span()?);
        Ok(Locatable::new(
            location,
            StructDeclaration {
                declaration,
                members,
            },
        ))
    }

    pub fn parse_declaration(&mut self) -> ParseResult<Locatable<Declaration>> {
        let location = self.current_span()?;
        let specifier = self.parse_declaration_specifier()?;
        let declarator = self.parse_pre_declarator()?;
        let mut ident = match_token!(self, current, Token::Identifier(ident) => ident.clone());
        if ident.is_some() {
            self.advance()?;
        }
        let declarator = self.parse_array_declarator(declarator)?;
        let location = ident
            .as_ref()
            .map_or(location, |locatable| location.merge(locatable.location));
        Ok(Locatable {
            location,
            value: Declaration {
                specifier,
                declarator,
                ident,
            },
        })
    }

    pub fn parse_array_declarator(
        &mut self,
        dec: Locatable<Box<DeclaratorType>>,
    ) -> ParseResult<Locatable<Box<DeclaratorType>>> {
        if is!(self, current, Token::Symbol(Symbol::OpenSquare)) {
            let location = self.current_span()?;
            self.advance()?;
            let size = if let Some(Locatable {
                location,
                value: (integer, suffix),
            }) = match_token!(self, current, Token::Literal(Literal::Integer {value, suffix}) => (*value, suffix.clone()))
            {
                if suffix.is_some() {
                    self.report_error(CompilerError::CustomError(
                        "Suffixes in array sizes are not currently supported.".to_string(),
                        location,
                    ));
                    return Err(());
                }
                self.advance()?;
                Some(integer as usize)
            } else {
                None
            };
            confirm!(self, consume, Token::Symbol(Symbol::CloseSquare) => (), "]")?;
            let location = location.merge(self.current_span()?);
            let dec = Box::new(DeclaratorType::Array { of: dec, size });
            let dec = Locatable::new(location, dec);
            Ok(self.parse_array_declarator(dec)?)
        } else {
            Ok(dec)
        }
    }

    pub fn parse_pre_declarator(&mut self) -> ParseResult<Locatable<Box<DeclaratorType>>> {
        let location = self.current_span()?;
        if is!(self, current, Token::Symbol(Symbol::Star)) {
            self.advance()?;
            let to = self.parse_pre_declarator()?;
            Ok(Locatable::new(
                location,
                Box::new(DeclaratorType::Pointer { to }),
            ))
        } else {
            Ok(Locatable::new(location, Box::new(DeclaratorType::Base)))
        }
    }

    pub fn parse_declaration_specifier(&mut self) -> ParseResult<Locatable<DeclarationSpecifier>> {
        let span = self.current_span()?;
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
        loop {
            if let Some(type_specifier) =
                match_token!(self, current, |x|{TypeSpecifier::try_from(x)}, Ok(x) => x)
            {
                type_specifiers.push(type_specifier.value);
                self.advance()?;
            } else if is!(self, current, Token::Keyword(Keyword::Struct)) {
                self.advance()?;
                let ident = self.confirm_identifier()?;
                type_specifiers.push(TypeSpecifier::Struct(ident.value));
            } else {
                break;
            }
        }
        let span = span.extend(self.current_span()?);
        Ok(Locatable {
            location: span,
            value: DeclarationSpecifier {
                specifiers: storage_specifiers,
                qualifiers: type_qualifiers,
                ty: type_specifiers,
            },
        })
    }

    pub fn parse_function_declaration(
        &mut self,
        declaration: Locatable<Declaration>,
    ) -> ParseResult<Locatable<FunctionDeclaration>> {
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
        let body = self.parse_compound_statement()?;
        let location = declaration.location.merge(body.location);
        Ok(Locatable::new(
            location,
            FunctionDeclaration {
                declaration,
                parameters,
                body,
            },
        ))
    }

    pub fn parse_variable_declaration(
        &mut self,
        declaration: Locatable<Declaration>,
    ) -> ParseResult<Locatable<VariableDeclaration>> {
        // debug_assert!(declaration.name.is_some());
        let initializer = if is!(self, current, Token::Symbol(Symbol::Equal)) {
            self.advance()?;
            Some(self.parse_initializer()?)
        } else {
            None
        };
        let location = initializer.as_ref().map_or(declaration.location, |init| {
            declaration.location.merge(init.location)
        });
        Ok(Locatable::new(
            location,
            VariableDeclaration {
                declaration,
                initializer,
            },
        ))
    }

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
            Token::Keyword(Keyword::Continue) => Ok(self.consume()?.map(|_| Statement::Continue)),
            Token::Keyword(Keyword::Break) => Ok(self.consume()?.map(|_| Statement::Break)),
            Token::Symbol(Symbol::OpenCurly) => {
                let block = self.parse_compound_statement()?;
                let location = block.location;
                Ok(Locatable::new(location, Statement::Block(block)))
            }
            Token::Keyword(keyword) if keyword.is_for_type() => {
                let dec = self.parse_declaration()?;
                let variable_declaration = self.parse_variable_declaration(dec)?;
                confirm!(self, consume, Token::Symbol(Symbol::Semicolon) => (), ";")?;
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

    fn parse_initializer(&mut self) -> ParseResult<Locatable<Expression>> {
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
}
impl<'a, L, E> Parser<'a, L, E>
where
    L: Iterator<Item = LexResult> + From<ArcStr>,
    E: ErrorReporter,
{
    pub fn parse_binary_expression(
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

    pub fn parse_prefix_unary_expression(&mut self) -> ParseResult<Locatable<Expression>> {
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

    pub fn create_unop(&mut self, un_op: UnaryOp) -> ParseResult<Locatable<Expression>> {
        let location = self.current_span()?;
        self.advance()?;
        let expr = self.parse_prefix_unary_expression()?;
        let location = location.merge(expr.location);
        let expr = Expression::Unary(un_op, expr.map(Box::new));
        let locatable = Locatable::new(location, expr);
        Ok(locatable)
    }

    pub fn parse_cast(&mut self) -> ParseResult<Locatable<Expression>> {
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

    pub fn parse_sizeof(&mut self) -> ParseResult<Locatable<Expression>> {
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

    pub fn parse_primary_expression(&mut self) -> ParseResult<Locatable<Expression>> {
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

    pub fn parse_postfix_unary_expression(
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

    /// This function parses call syntax and is initiated by an open parenthesis placed immediately
    /// after a primary expression. It is placed in its own function to keep "parse_postfix_unary_expression"
    /// more concise. This expects a primary_expr as input in order to nest the original
    /// expression into the AST node. This currently only parses calls on identifiers because the
    /// compiler does not support function pointers yet.
    pub fn parse_function_call(
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

    pub fn parse_index_access(
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

    pub fn parse_member_access(
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

    pub fn parse_deref_member_access(
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
//
// #[test]
// fn test_advance_advances_tokens_correctly() {
//     let program = Compiler::new("test".to_string());
//     let source = "1 * 1".to_string();
//     let lexer = crate::lex::Lexer::new(source);
//     let mut parser = Parser::from_lexer(program, lexer);
//     parser.prime();
//     assert!(parser.consume().is_ok());
//     assert!(parser.consume().is_ok());
//     assert!(parser.consume().is_ok());
//     assert!(parser.consume().is_err());
// }
