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

    Integer(u64),
    FloatLiteral(f64),

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

impl TokenKind {
    pub fn match_symbol(symbol: &str) -> Option<TokenKind> {
        use TokenKind::*;
        match symbol {
            "sizeof" => Some(Sizeof),
            "+" => Some(Plus),
            "+=" => Some(PlusEqual),
            "++" => Some(Increment),
            "-" => Some(Minus),
            "->" => Some(Arrow),
            "-=" => Some(MinusEqual),
            "--" => Some(Decrement),
            "*" => Some(Star),
            "*=" => Some(StarEqual),
            "/" => Some(Slash),
            "/=" => Some(SlashEqual),
            "%" => Some(Modulo),
            "%=" => Some(ModuloEqual),
            "=" => Some(Equal),
            "==" => Some(EqualEqual),
            "!" => Some(Bang),
            "!=" => Some(BangEqual),
            "|" => Some(Pipe),
            "|=" => Some(PipeEqual),
            "||" => Some(DoublePipe),
            "&" => Some(Ampersand),
            "&=" => Some(AmpersandEqual),
            "&&" => Some(DoubleAmpersand),
            "^" => Some(Caret),
            "^=" => Some(CaretEqual),
            "~" => Some(Tilde),
            "<<" => Some(LeftShift),
            "<<=" => Some(LeftShiftEqual),
            ">>" => Some(RightShift),
            ">>=" => Some(RightShiftEqual),
            ">" => Some(GreaterThan),
            ">=" => Some(GreaterThanEqual),
            "<" => Some(LessThan),
            "<=" => Some(LessThanEqual),
            "?" => Some(QuestionMark),
            ":" => Some(Colon),
            "," => Some(Comma),
            "." => Some(Dot),
            "[" => Some(OpenSquare),
            "]" => Some(CloseSquare),
            "{" => Some(OpenCurly),
            "}" => Some(CloseCurly),
            "(" => Some(OpenParen),
            ")" => Some(CloseParen),
            ";" => Some(Semicolon),
            _ => None,
        }
    }
}
