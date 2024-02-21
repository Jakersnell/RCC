use crate::str_intern::InternedStr;
use std::fmt::Display;
use std::sync::Arc;

#[derive(Debug, PartialEq)]
pub enum Token {
    BadSymbol(char),
    Identifier(InternedStr),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer { value: u128, suffix: Option<String> },
    Float { value: f64, suffix: Option<String> },
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Int,
    Double,
    Return,
}

impl Keyword {
    pub fn is_type(&self) -> bool {
        matches!(self, Keyword::Int | Keyword::Double)
    }
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Sizeof, // It's really convenient to have this as a symbol

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
}
