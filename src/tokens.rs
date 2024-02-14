use arcstr::Substr;
use derive_new::new;
use std::{default, error, sync::Arc};
use thiserror::Error as ErrorType;

use crate::error::CompilerError;

#[derive(Debug, PartialEq, new)]
pub struct Locatable<T> {
    pub location: Span,
    pub value: T,
}

#[derive(Debug, PartialEq, new)]
pub struct Span {
    start: usize,
    end: usize,
}


#[derive(Debug, PartialEq)]
pub enum Token {
    BadSymbol(char),
    Identifier(String),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),

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

    Sizeof
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(u64),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Int,
    Double,
    Return,
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    Sizeof,

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
