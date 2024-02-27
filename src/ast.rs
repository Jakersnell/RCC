use crate::ast::PostfixOp::Increment;
use crate::str_intern::InternedStr;
use crate::tokens::{Keyword, Literal, Symbol, Token};
use crate::util::CompilerResult;
use std::fmt::Display;
use std::sync::Arc;

/*
Some things in this AST very closely follow the ANSI C Yacc grammar.
The majority does not.
 */

pub type ASTRoot = Vec<InitDeclaration>;

#[derive(Debug)]
pub(crate) struct Block(pub(crate) Vec<Statement>);

#[derive(Debug)]
pub(crate) enum InitDeclaration {
    Declaration(VariableDeclaration), // (declaration,  initializer)
    Function(FunctionDeclaration),
}

#[derive(Debug)]
pub(crate) struct FunctionDeclaration {
    pub(crate) declaration: Declaration,
    pub(crate) parameters: Vec<Declaration>,
    pub(crate) varargs: bool,       // we don't currently support varargs
    pub(crate) body: Option<Block>, // this is an option, but we currently don't support function prototypes
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub(crate) declaration: Declaration,
    pub(crate) initializer: Option<Expression>,
}

#[derive(Debug)]
pub(crate) struct Declaration {
    pub(crate) specifier: DeclarationSpecifier,
    pub(crate) declarator: Declarator,
}

#[derive(Debug)]
pub(crate) enum Declarator {
    // not supporting function pointers
    Pointer {
        to: Box<Declarator>,
    },
    Array {
        of: Box<Declarator>,
        size: Option<usize>,
    },
    Unit {
        ident: InternedStr,
    },
    None,
}

#[derive(Debug)]
pub(crate) struct DeclarationSpecifier {
    pub(crate) specifiers: Vec<StorageSpecifier>,
    pub(crate) qualifiers: Vec<TypeQualifier>,
    pub(crate) ty: Vec<TypeSpecifier>,
}

#[derive(Debug)]
pub(crate) enum TypeSpecifier {
    Void,
    Char,
    Int,
    Long,
    Double,
    Signed,
    Unsigned,
}

impl TryFrom<&Token> for TypeSpecifier {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use TypeSpecifier::*;
        match value {
            Token::Keyword(Keyword::Int) => Ok(Int),
            Token::Keyword(Keyword::Double) => Ok(Double),
            Token::Keyword(Keyword::Void) => Ok(Void),
            Token::Keyword(Keyword::Char) => Ok(Char),
            Token::Keyword(Keyword::Long) => Ok(Long),
            Token::Keyword(Keyword::Signed) => Ok(Signed),
            Token::Keyword(Keyword::Unsigned) => Ok(Unsigned),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum StorageSpecifier {
    Static,
}

impl TryFrom<&Token> for StorageSpecifier {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use StorageSpecifier::*;
        match value {
            Token::Keyword(Keyword::Static) => Ok(Static),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum TypeQualifier {
    Const,
}

impl TryFrom<&Token> for TypeQualifier {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use TypeQualifier::*;
        match value {
            Token::Keyword(Keyword::Const) => Ok(Const),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Declaration(VariableDeclaration),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    For(
        Option<VariableDeclaration>,
        Option<Expression>,
        Option<Expression>,
        Box<Statement>,
    ),
    Break,
    Continue,
    Return(Option<Expression>),
    Block(Block),
}

#[derive(Debug)]
pub(crate) enum Expression {
    Literal(Literal),
    Variable(InternedStr),
    Sizeof(TypeOrExpression),
    // this doesn't include all postfix operations, just inc and dec
    PostFix(PostfixOp, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Assignment(AssignOp, InternedStr, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    FunctionCall(InternedStr, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Member(Box<Expression>, InternedStr),
    PointerMember(Box<Expression>, InternedStr),
    Cast(TypeSpecifier, Box<Expression>),
}

#[derive(Debug)]
pub(crate) enum TypeOrExpression {
    Type(DeclarationSpecifier),
    Expr(Box<Expression>),
}

#[derive(Debug)]
pub(crate) enum PostfixOp {
    Increment,
    Decrement,
}

impl TryFrom<&Token> for PostfixOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use PostfixOp::*;
        match value {
            Token::Symbol(Symbol::Increment) => Ok(Increment),
            Token::Symbol(Symbol::Decrement) => Ok(Decrement),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum UnaryOp {
    Increment,
    Decrement,
    Plus,
    Negate,
    LogicalNot,
    BitwiseNot,
}

impl TryFrom<&Token> for UnaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use UnaryOp::*;
        match value {
            Token::Symbol(Symbol::Plus) => Ok(Plus),
            Token::Symbol(Symbol::Minus) => Ok(Negate),
            Token::Symbol(Symbol::Bang) => Ok(LogicalNot),
            Token::Symbol(Symbol::Tilde) => Ok(BitwiseNot),
            Token::Symbol(Symbol::Increment) => Ok(Increment),
            Token::Symbol(Symbol::Decrement) => Ok(Decrement),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    LeftShift,
    RightShift,

    Assign(AssignOp),
}

impl BinaryOp {
    pub(crate) fn precedence(&self) -> u8 {
        use BinaryOp::*;
        match self {
            Multiply | Divide | Modulo => 3,
            Add | Subtract => 2,
            _ => 0,
        }
    }
}

impl TryFrom<&Token> for BinaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use BinaryOp::*;
        match value {
            Token::Symbol(Symbol::Plus) => Ok(Add),
            Token::Symbol(Symbol::Minus) => Ok(Subtract),
            Token::Symbol(Symbol::Star) => Ok(Multiply),
            Token::Symbol(Symbol::Slash) => Ok(Divide),
            Token::Symbol(Symbol::Modulo) => Ok(Modulo),

            Token::Symbol(Symbol::EqualEqual) => Ok(Equal),
            Token::Symbol(Symbol::BangEqual) => Ok(NotEqual),
            Token::Symbol(Symbol::GreaterThan) => Ok(GreaterThan),
            Token::Symbol(Symbol::GreaterThanEqual) => Ok(GreaterThanEqual),
            Token::Symbol(Symbol::LessThan) => Ok(LessThan),
            Token::Symbol(Symbol::LessThanEqual) => Ok(LessThanEqual),

            Token::Symbol(Symbol::Ampersand) => Ok(BitwiseAnd),
            Token::Symbol(Symbol::Pipe) => Ok(BitwiseOr),
            Token::Symbol(Symbol::Caret) => Ok(BitwiseXor),
            Token::Symbol(Symbol::LeftShift) => Ok(LeftShift),
            Token::Symbol(Symbol::RightShift) => Ok(RightShift),

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum AssignOp {
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}
impl TryFrom<&Token> for AssignOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use AssignOp::*;
        match value {
            Token::Symbol(Symbol::Equal) => Ok(Assign),
            Token::Symbol(Symbol::PlusEqual) => Ok(Plus),
            Token::Symbol(Symbol::MinusEqual) => Ok(Minus),
            Token::Symbol(Symbol::StarEqual) => Ok(Multiply),
            Token::Symbol(Symbol::SlashEqual) => Ok(Divide),
            Token::Symbol(Symbol::ModuloEqual) => Ok(Modulo),
            Token::Symbol(Symbol::AmpersandEqual) => Ok(BitwiseAnd),
            Token::Symbol(Symbol::PipeEqual) => Ok(BitwiseOr),
            Token::Symbol(Symbol::CaretEqual) => Ok(BitwiseXor),
            Token::Symbol(Symbol::LeftShiftEqual) => Ok(LeftShift),
            Token::Symbol(Symbol::RightShiftEqual) => Ok(RightShift),
            _ => Err(()),
        }
    }
}
