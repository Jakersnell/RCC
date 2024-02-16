use derive_new::new;

use crate::{error::CompilerError, tokens::{Literal, Symbol}};

#[derive(Debug)]
pub struct Program {
    pub filename: String,
    pub body: Result<Vec<ASTNode>, Vec<CompilerError>>,
}


#[derive(Debug)]
pub enum ASTNode {
    Statement(Statement),
    Expression(Expression),
    Function(Function),
}

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Block(Vec<ASTNode>),
    Return(Expression),
}

#[derive(Debug, new)]
pub struct VariableDeclaration {
    pub name: String,
    pub ty: DataType,
}

#[derive(Debug, new)]
pub enum Expression {
    Variable(String),
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    FunctionCall(FunctionCall),
}

#[derive(Debug, new)]
pub struct UnaryExpression {
    op: UnOp,
    right: Box<Expression>,
}

#[derive(Debug, new)]
pub struct BinaryExpression {
    left: Box<Expression>,
    op: BinOp,
    right: Box<Expression>,
}


#[derive(Debug, new)]
pub struct Function {
    pub name: String,
    pub params: Vec<VariableDeclaration>,
    pub body: Box<Vec<ASTNode>>,
}

#[derive(Debug, new)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<ASTNode>,
}

#[derive(Debug)]
pub enum BinOp {
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

#[derive(Debug)]
pub enum AssignOp {
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

impl BinOp {
    pub fn is_assignment(&self) -> bool {
        match self {
            BinOp::Assign(_) => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> u8 {
        use BinOp::*;
        match self {
            Multiply | Divide | Modulo => 3,
            Add | Subtract => 2,
            Assign(_) => 1,
            _ => panic!("Invalid precedence for {:?}", self),
        }
    }

}

impl TryFrom<Symbol> for BinOp {
    type Error = ();

    fn try_from(value: Symbol) -> Result<Self, Self::Error> {
        match value {
            Symbol::Plus => Ok(BinOp::Add),
            Symbol::Minus => Ok(BinOp::Subtract),
            Symbol::Star => Ok(BinOp::Multiply),
            Symbol::Slash => Ok(BinOp::Divide),
            Symbol::Modulo => Ok(BinOp::Modulo),

            Symbol::EqualEqual => Ok(BinOp::Equal),
            Symbol::BangEqual => Ok(BinOp::NotEqual),
            Symbol::GreaterThan => Ok(BinOp::GreaterThan),
            Symbol::GreaterThanEqual => Ok(BinOp::GreaterThanEqual),
            Symbol::LessThan => Ok(BinOp::LessThan),
            Symbol::LessThanEqual => Ok(BinOp::LessThanEqual),

            Symbol::Ampersand => Ok(BinOp::BitwiseAnd),
            Symbol::Pipe => Ok(BinOp::BitwiseOr),
            Symbol::Caret => Ok(BinOp::BitwiseXor),
            Symbol::LeftShift => Ok(BinOp::LeftShift),
            Symbol::RightShift => Ok(BinOp::RightShift),

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum UnOp {
    Negate,
    LogicalNot,
    BitwiseNot,
    Sizeof,
}

impl TryFrom<Symbol> for UnOp {
    type Error = ();

    fn try_from(value: Symbol) -> Result<Self, Self::Error> {
        match value {
            Symbol::Minus => Ok(UnOp::Negate),
            Symbol::Bang => Ok(UnOp::LogicalNot),
            Symbol::Tilde => Ok(UnOp::BitwiseNot),
            Symbol::Sizeof => Ok(UnOp::Sizeof),

            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum DataType {
    Int,
    UnsignedInt,
    Float,
}
