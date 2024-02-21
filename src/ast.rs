use std::fmt::Display;
use std::sync::Arc;

use derive_new::new;
use thiserror::__private::AsDisplay;

use crate::tokens::{Literal, Symbol, Token};
use crate::util::{CompoundExpression, DeclarationNode, ExpressionNode, StatementNode};

#[derive(Debug)]
pub enum ASTNode {
    Statement(StatementNode),
    Expression(ExpressionNode),
    Declaration(DeclarationNode),
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
    Block(Vec<ASTNode>),
    Return(Expression),
}

#[derive(Debug, new)]
pub enum Expression {
    Variable(String),
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    FunctionCall(FunctionCall),
    SizeOf(TypeOrIdentifier),
}

#[derive(Debug)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Variable(VariableDeclaration),
}

#[derive(Debug, new)]
pub struct VariableDeclaration {
    pub name: String,
    pub ty: DataType,
}

#[derive(Debug, new)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: DataType,
    pub params: Vec<VariableDeclaration>,
    pub var_args: bool,
    pub body: Option<Vec<ASTNode>>,
}

#[derive(Debug, new)]
pub struct UnaryExpression {
    pub op: UnOp,
    pub right: Box<ExpressionNode>,
}

#[derive(Debug, new)]
pub struct BinaryExpression {
    pub left: Box<ExpressionNode>,
    pub op: BinOp,
    pub right: Box<ExpressionNode>,
}

#[derive(Debug, new)]
pub struct FunctionCall {
    pub name: String,
    pub args: CompoundExpression,
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

#[derive(Debug)]
pub enum UnOp {
    Plus,
    Negate,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug)]
pub enum TypeOrIdentifier {
    Type(DataType),
    Identifier(Arc<String>),
}

#[derive(Debug)]
pub enum DataType {
    Int,
    UnsignedInt,
    Float,
}

impl BinOp {
    pub fn is_assignment(&self) -> bool {
        matches!(self, BinOp::Assign(_))
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinOp::*;
        let str = match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            Equal => "==",
            NotEqual => "!=",
            GreaterThan => ">",
            GreaterThanEqual => ">=",
            LessThan => "<",
            LessThanEqual => "<=",
            LogicalAnd => "&&",
            LogicalOr => "||",
            BitwiseAnd => "&",
            BitwiseOr => "|",
            BitwiseXor => "^",
            LeftShift => "<<",
            RightShift => ">>",
            Assign(AssignOp::Assign) => "=",
            Assign(AssignOp::Plus) => "+=",
            Assign(AssignOp::Minus) => "-=",
            Assign(AssignOp::Multiply) => "*=",
            Assign(AssignOp::Divide) => "/=",
            Assign(AssignOp::Modulo) => "%=",
            Assign(AssignOp::BitwiseAnd) => "&=",
            Assign(AssignOp::BitwiseOr) => "|=",
            Assign(AssignOp::BitwiseXor) => "^=",
            Assign(AssignOp::LeftShift) => "<<=",
            Assign(AssignOp::RightShift) => ">>=",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl TryFrom<&Token> for BinOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Symbol(Symbol::Plus) => Ok(BinOp::Add),
            Token::Symbol(Symbol::Minus) => Ok(BinOp::Subtract),
            Token::Symbol(Symbol::Star) => Ok(BinOp::Multiply),
            Token::Symbol(Symbol::Slash) => Ok(BinOp::Divide),
            Token::Symbol(Symbol::Modulo) => Ok(BinOp::Modulo),

            Token::Symbol(Symbol::EqualEqual) => Ok(BinOp::Equal),
            Token::Symbol(Symbol::BangEqual) => Ok(BinOp::NotEqual),
            Token::Symbol(Symbol::GreaterThan) => Ok(BinOp::GreaterThan),
            Token::Symbol(Symbol::GreaterThanEqual) => Ok(BinOp::GreaterThanEqual),
            Token::Symbol(Symbol::LessThan) => Ok(BinOp::LessThan),
            Token::Symbol(Symbol::LessThanEqual) => Ok(BinOp::LessThanEqual),

            Token::Symbol(Symbol::Ampersand) => Ok(BinOp::BitwiseAnd),
            Token::Symbol(Symbol::Pipe) => Ok(BinOp::BitwiseOr),
            Token::Symbol(Symbol::Caret) => Ok(BinOp::BitwiseXor),
            Token::Symbol(Symbol::LeftShift) => Ok(BinOp::LeftShift),
            Token::Symbol(Symbol::RightShift) => Ok(BinOp::RightShift),

            _ => Err(()),
        }
    }
}

impl UnOp {
    pub fn precedence(&self) -> u8 {
        use UnOp::*;
        match self {
            Plus => 4,
            Negate => 4,
            LogicalNot => 4,
            BitwiseNot => 4,
        }
    }
}

impl TryFrom<&Token> for UnOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Symbol(Symbol::Plus) => Ok(UnOp::Plus),
            Token::Symbol(Symbol::Minus) => Ok(UnOp::Negate),
            Token::Symbol(Symbol::Bang) => Ok(UnOp::LogicalNot),
            Token::Symbol(Symbol::Tilde) => Ok(UnOp::BitwiseNot),
            _ => Err(()),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnOp::*;
        let str = match self {
            Plus => "+",
            Negate => "-",
            LogicalNot => "!",
            BitwiseNot => "~",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl TryFrom<Token> for AssignOp {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::Symbol(Symbol::Equal) => Ok(AssignOp::Assign),
            Token::Symbol(Symbol::PlusEqual) => Ok(AssignOp::Plus),
            Token::Symbol(Symbol::MinusEqual) => Ok(AssignOp::Minus),
            Token::Symbol(Symbol::StarEqual) => Ok(AssignOp::Multiply),
            Token::Symbol(Symbol::SlashEqual) => Ok(AssignOp::Divide),
            Token::Symbol(Symbol::ModuloEqual) => Ok(AssignOp::Modulo),
            Token::Symbol(Symbol::AmpersandEqual) => Ok(AssignOp::BitwiseAnd),
            Token::Symbol(Symbol::PipeEqual) => Ok(AssignOp::BitwiseOr),
            Token::Symbol(Symbol::CaretEqual) => Ok(AssignOp::BitwiseXor),
            Token::Symbol(Symbol::LeftShiftEqual) => Ok(AssignOp::LeftShift),
            Token::Symbol(Symbol::RightShiftEqual) => Ok(AssignOp::RightShift),

            _ => Err(()),
        }
    }
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AssignOp::*;
        let str = match self {
            Assign => "=",
            Plus => "+=",
            Minus => "-=",
            Multiply => "*=",
            Divide => "/=",
            Modulo => "%=",
            BitwiseAnd => "&=",
            BitwiseOr => "|=",
            BitwiseXor => "^=",
            LeftShift => "<<=",
            RightShift => ">>=",
        }
        .to_string();
        write!(f, "{}", str)
    }
}
