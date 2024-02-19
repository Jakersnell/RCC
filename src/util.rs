use crate::error::CompilerError;
use crate::tokens::Token as LexToken;
use derive_new::new;

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

pub type Token = Locatable<LexToken>;
pub type CompilerResult<T> = Result<T, Vec<Locatable<CompilerError>>>;
