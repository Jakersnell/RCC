use crate::tokens::{Literal, Symbol};

#[derive(Debug)]
pub struct SyntaxTree {
    pub global: Vec<Node>,
}

#[derive(Debug)]
pub enum Node {
    Type(DataType),
    LiteralExpr(Literal),
    BinaryExpr {
        left: Box<Node>,
        op: BinOp,
        right: Box<Node>,
    },
    UnaryExpr {
        op: UnOp,
        right: Box<Node>,
    },
    VariableDeclaration {
        name: String,
        ty: DataType,
        value: Option<Box<Node>>,
    },
    FunctionDeclaration {
        name: String,
        params: Vec<Node>,
        body: Box<Node>,
    },
    FunctionCall {
        name: String,
        args: Vec<Node>,
    },
    Block {
        statements: Vec<Node>,
    },
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
