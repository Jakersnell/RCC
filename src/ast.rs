use crate::token::Token;

#[derive(Debug)]
pub struct SyntaxTree {
    pub root: Node,
    pub eof: Token,
}

#[derive(Debug)]
pub enum Node {
    LiteralExpr(Literal),
    BinaryExpr {
        left: Box<Node>,
        op: TokenKind,
        right: Box<Node>,
    },
    UnaryExpr {
        op: TokenKind,
        right: Box<Node>,
    },
    VariableDeclaration {
        name: String,
        ty: DataType,
        value: Option<Box<Node>>,
    },
    FunctionDeclaration {
        name: String,
        params: Vec<VariableDeclaration>,
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
}

pub enum UnOp {
    Negate,
    LogicalNot,
    BitwiseNot,
    Sizeof
}

pub enum DataType {
    Int,
    UnsignedInt,
    Float,
}
