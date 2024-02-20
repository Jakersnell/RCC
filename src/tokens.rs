#[derive(Debug, PartialEq)]
pub enum Token {
    BadSymbol(char),
    Identifier(String),
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
    Sizeof,
}

impl Keyword {
    pub fn is_type(&self) -> bool {
        matches!(self, Keyword::Int | Keyword::Double)
    }
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
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

#[test]
fn test_token_as_locatable_value_can_be_equivalency_checked() {
    let span = crate::util::Span::new(0, 1);
    let token = Token::Keyword(Keyword::Int);
    let locatable = Some(crate::util::Locatable::new(span, token));
    let boolean = locatable
        .as_ref()
        .is_some_and(|locatable| matches!(&locatable.value, Token::Keyword(x) if x.is_type()));
    assert!(boolean);
}
