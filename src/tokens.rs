use crate::str_intern::InternedStr;
use arcstr::Substr;
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

impl Token {
    pub fn is_assign_op(&self) -> bool {
        matches!(
            self,
            Token::Symbol(Symbol::Equal)
                | Token::Symbol(Symbol::PlusEqual)
                | Token::Symbol(Symbol::MinusEqual)
                | Token::Symbol(Symbol::StarEqual)
                | Token::Symbol(Symbol::SlashEqual)
                | Token::Symbol(Symbol::ModuloEqual)
                | Token::Symbol(Symbol::AmpersandEqual)
                | Token::Symbol(Symbol::PipeEqual)
                | Token::Symbol(Symbol::CaretEqual)
                | Token::Symbol(Symbol::LeftShiftEqual)
                | Token::Symbol(Symbol::RightShiftEqual)
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer { value: u128, suffix: Option<String> },
    Float { value: f64, suffix: Option<String> },
    Char { value: char },
    String { value: Substr },
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Keyword {
    Void,
    Char,
    Long,
    Int,
    Double,
    Return,
    Signed,
    Unsigned,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Static,
    Const,
}

impl Keyword {
    pub fn is_type(&self) -> bool {
        matches!(self, Keyword::Int | Keyword::Double | Keyword::Void)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
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
