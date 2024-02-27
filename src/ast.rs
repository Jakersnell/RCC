use crate::ast::PostfixOp::Increment;
use crate::str_intern::InternedStr;
use crate::tokens::{Literal, Symbol, Token};
use crate::util::CompilerResult;
use std::fmt::Display;
use std::sync::Arc;

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
    pub(crate) varargs: bool, // varargs == true means the last element is of type parameters[parameters.len()-1]
    pub(crate) body: Option<Block>,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub(crate) declaration: Declaration,
    pub(crate) initializer: Option<Expression>,
}

#[derive(Debug)]
pub(crate) struct Declaration {
    pub(crate) ty: DeclarationType,
    pub(crate) name: Option<InternedStr>,
}

#[derive(Debug)]
pub(crate) enum DeclarationType {
    Pointer {
        to: Box<DeclarationType>,
    },
    Array {
        of: Box<DeclarationType>,
        size: Option<usize>,
    },
    Unit {
        specifiers: Option<Vec<Specifier>>,
        ty: DataType,
    },
}

#[derive(Debug)]
pub(crate) enum DataType {
    Void,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,
}

impl TryFrom<&Token> for DataType {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use DataType::*;
        match value {
            Token::Keyword(keyword) => match keyword {
                crate::tokens::Keyword::Int => Ok(Int),
                crate::tokens::Keyword::Double => Ok(Double),
                _ => Err(()),
            },
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Specifier {
    Const,
    Unsigned,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Declaration(VariableDeclaration),
    Return(Option<Expression>),
    Block(Block),
}

#[derive(Debug)]
pub(crate) enum Expression {
    Literal(Literal),
    Variable(InternedStr),
    Sizeof(TypeOrExpression),
    // this doesn't include all postfix operations, just inc and dec so far
    PostFix(PostfixOp, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Assignment(AssignOp, InternedStr, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    FunctionCall(InternedStr, Vec<Expression>),
}

#[derive(Debug)]
pub(crate) enum TypeOrExpression {
    Type(DataType),
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
