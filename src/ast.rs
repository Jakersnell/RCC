use crate::tokens::{Literal, Symbol, Token};
use std::fmt::Display;
use std::sync::Arc;

#[derive(Debug)]
pub(crate) struct Block(pub(crate) Vec<Statement>);

#[derive(Debug)]
pub(crate) enum InitDeclaration {
    Declaration(Declaration, Option<Expression>), // (declaration,  initializer)
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
pub(crate) struct Declaration {
    pub(crate) ty: DeclarationType,
    pub(crate) name: Option<Arc<str>>,
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
    Type {
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
    Declaration(Declaration),
    Return(Option<Expression>),
    Block(Block),
}

#[derive(Debug)]
pub(crate) enum Expression {
    Literal(Literal),
    Variable(Arc<str>),
    Sizeof(TypeOrIdentifier),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    FunctionCall(Arc<str>, Vec<Expression>),
}

impl Expression {
    pub(crate) fn pretty_print(&self, padding: String, last: bool) -> String {
        let output = format!("{}{}", padding, if last { "└──" } else { "├──" });
        let mut padding = padding;
        padding += if last { "   " } else { "│  " };
        let child_output = match self {
            Expression::Variable(ident) => format!(" {}\n", ident),
            Expression::Literal(literal) => {
                let value = match literal {
                    Literal::Integer { value, suffix } => value.to_string(),
                    Literal::Float { value, suffix } => value.to_string(),
                };
                format!(" {}\n", value)
            }
            Expression::Binary(op, left, right) => {
                let left = left.pretty_print(padding.clone(), false);
                let right = right.pretty_print(padding.clone(), true);
                format!(" {}\n{}{}", op, left, right)
            }
            Expression::Unary(op, right) => {
                right.pretty_print(padding.clone(), true);
                format!(" {}\n", op)
            }
            val => unimplemented!("{:#?}", val),
        };

        output + &child_output
    }
}

#[derive(Debug)]
pub(crate) enum TypeOrIdentifier {
    Type(DataType),
    Identifier(Arc<str>),
}

#[derive(Debug)]
pub(crate) enum UnaryOp {
    Plus,
    Negate,
    LogicalNot,
    BitwiseNot,
}

impl UnaryOp {
    pub(crate) fn precedence(&self) -> u8 {
        use UnaryOp::*;
        match self {
            Plus => 4,
            Negate => 4,
            LogicalNot => 4,
            BitwiseNot => 4,
        }
    }
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
            Assign(_) => 1,
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

            Token::Symbol(Symbol::Equal) => Ok(Assign(AssignOp::Assign)),
            Token::Symbol(Symbol::PlusEqual) => Ok(Assign(AssignOp::Plus)),
            Token::Symbol(Symbol::MinusEqual) => Ok(Assign(AssignOp::Minus)),
            Token::Symbol(Symbol::StarEqual) => Ok(Assign(AssignOp::Multiply)),
            Token::Symbol(Symbol::SlashEqual) => Ok(Assign(AssignOp::Divide)),
            Token::Symbol(Symbol::ModuloEqual) => Ok(Assign(AssignOp::Modulo)),
            Token::Symbol(Symbol::AmpersandEqual) => Ok(Assign(AssignOp::BitwiseAnd)),
            Token::Symbol(Symbol::PipeEqual) => Ok(Assign(AssignOp::BitwiseOr)),
            Token::Symbol(Symbol::CaretEqual) => Ok(Assign(AssignOp::BitwiseXor)),
            Token::Symbol(Symbol::LeftShiftEqual) => Ok(Assign(AssignOp::LeftShift)),
            Token::Symbol(Symbol::RightShiftEqual) => Ok(Assign(AssignOp::RightShift)),

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
