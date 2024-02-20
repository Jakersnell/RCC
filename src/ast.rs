use std::sync::Arc;

use derive_new::new;

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
    op: UnOp,
    right: Box<ExpressionNode>,
}

#[derive(Debug, new)]
pub struct BinaryExpression {
    left: Box<ExpressionNode>,
    op: BinOp,
    right: Box<ExpressionNode>,
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

#[derive(Debug)]
pub enum UnOp {
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

impl TryFrom<Symbol> for UnOp {
    type Error = ();

    fn try_from(value: Symbol) -> Result<Self, Self::Error> {
        match value {
            Symbol::Minus => Ok(UnOp::Negate),
            Symbol::Bang => Ok(UnOp::LogicalNot),
            Symbol::Tilde => Ok(UnOp::BitwiseNot),
            _ => Err(()),
        }
    }
}
