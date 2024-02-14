use arcstr::Substr;
use derive_new::new;
use std::{default, error, sync::Arc};
use thiserror::Error as ErrorType;

#[derive(Debug, PartialEq, ErrorType)]
pub enum TokenProblem {
    #[error("{0}")]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error("{0}")]
    ParseFloatError(#[from] std::num::ParseFloatError),
    #[error("{0} is an invalid hex Literal")]
    InvalidHexLiteral(String),
    #[error("Invalid character.")]
    InvalidCharacter,
}

#[derive(Debug, new)]
pub struct LexToken {
    pub kind: TokenKind,
    pub location: Span,
    pub problems: Vec<TokenProblem>,
}

#[derive(Debug, PartialEq, new)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, PartialEq, Default)]
pub enum TokenKind {
    #[default]
    BadToken,

    Identifier(String),
    Integer(u64),
    FloatLiteral(f64),

    IntKeyword,
    DoubleKeyword,
    ReturnKeyword,

    Plus,
    Minus,
    Star,
    Slash,
    Modulo,

    EqualEqual,
    BangEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    Bang,
    DoubleAmpersand,
    DoublePipe,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LeftShift,
    RightShift,

    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    ModuloEqual,
    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    LeftShiftEqual,
    RightShiftEqual,

    Increment,
    Decrement,

    QuestionMark,
    Colon,
    Comma,
    Dot,
    Arrow,

    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Semicolon,

    Sizeof,
}

pub enum Keyword {
    Int,
    Double,
    Return,
}

pub enum Literal {
    Integer(u64),
    Float(f64),
}

pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,

    EqualEqual,
    BangEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    Bang,
    DoubleAmpersand,
    DoublePipe,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LeftShift,
    RightShift,

    Equal,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    ModuloEqual,
    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    LeftShiftEqual,
    RightShiftEqual,

    Increment,
    Decrement,

    QuestionMark,
    Colon,
    Comma,
    Dot,
    Arrow,
}

pub enum Symbol {
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Semicolon,
}
