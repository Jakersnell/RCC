use crate::util::Span;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("{0}")]
    IoError(#[from] std::io::Error),

    #[error("{0}")]
    ParseIntError(#[from] std::num::ParseIntError),

    #[error("{0}")]
    ParseFloatError(#[from] std::num::ParseFloatError),

    #[error("Invalid integer suffix: {0}")]
    InvalidIntegerSuffix(String, Span),

    #[error("Invalid float suffix: {0}")]
    InvalidFloatSuffix(String, Span),

    #[error("Invalid symbol: {0}")]
    InvalidSymbol(String, Span),

    #[error("Found `{0}` but expected one of the following: \n\t{1}\n")]
    ExpectedVariety(String, String, Span),

    #[error("Expected `{0}` but found {1}")]
    ExpectedButFound(String, String, Span),

    #[error("Invalid hex literal: {0}")]
    InvalidHexLiteral(Span),

    #[error("Invalid octal literal: {0}")]
    InvalidOctalLiteral(Span),

    #[error("Invalid binary literal: {0}")]
    InvalidBinaryLiteral(Span),

    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(Span),

    #[error("Invalid character literal: {0}")]
    InvalidCharacterLiteral(Span),

    #[error("Unclosed string literal: {0}")]
    UnclosedStringLiteral(Span),

    #[error("Unclosed char literal: {0}")]
    UnclosedCharLiteral(Span),

    #[error("Cannot cast {0} to {1}")]
    CannotCast(String, String, Span),

    #[error("Cannot assign {0} to type {1}")]
    CannotAssign(String, String, Span),

    #[error("Unknown identifier \"{0}\"")]
    UnknownIdentifier(String, Span),

    #[error("Must return type {0} due to declared type")]
    MustReturn(String, Span),

    #[error("Unclosed parenthesis")]
    UnclosedParenthesis,

    #[error("Unclosed block")]
    UnclosedBlock,

    #[error("Unclosed array")]
    UnclosedArray,

    #[error("Parenthesis has no opening.")]
    ParenthesisHasNoOpening,

    #[error("Curly has no opening.")]
    BlockHasNoOpening,

    #[error("Unexpected end of file.")]
    UnexpectedEOF,

    #[error("'This identifier already exists in this scope and cannot be redeclared.")]
    IdentifierExists(Span),

    #[error("{0}")]
    CustomError(String),

    #[error("Else without if")]
    ElseWithNoIf(Span),
}

#[derive(Error, Debug)]
pub enum CompilerWarning {
    #[error("Unused variable: {0}")]
    UnusedVariable(String),

    #[error("Unused function: {0}")]
    UnusedFunction(String),

    #[error("Unused parameter: {0}")]
    UnusedParameter(String),

    #[error("Unused constant: {0}")]
    UnusedConstant(String),

    #[error("Unused struct: {0}")]
    UnusedStruct(String),

    #[error("Unused enum: {0}")]
    UnusedEnum(String),

    #[error("Unused union: {0}")]
    UnusedUnion(String),

    #[error("Unreachable code: {0}")]
    UnreachableCode(String),
}
