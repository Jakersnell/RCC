use crate::ast::InitDeclaration;
use crate::error::{CompilerError, CompilerWarning};
use crate::tokens::Token as LexToken;
use derive_new::new;
use std::fmt::{Display, Formatter};

pub type LocatableToken = Locatable<LexToken>;
pub type CompilerResult<T> = Result<T, Vec<Locatable<CompilerError>>>;

#[derive(Debug, PartialEq, new)]
pub struct Locatable<T> {
    pub location: Span,
    pub value: T,
}

impl<T> Locatable<T> {
    #[inline]
    pub fn map<F, U>(self, mapper: F) -> Locatable<U>
    where
        F: Fn(T) -> U,
    {
        Locatable::new(self.location, mapper(self.value))
    }
}

#[derive(Debug, PartialEq, new, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub col: usize,
    pub line: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Span({}, {})", self.start, self.end)
    }
}

#[derive(Debug)]
pub struct Program {
    // I plan on adding more fields to this struct later
    pub body: Option<CompilerResult<Vec<InitDeclaration>>>,
    pub warnings: Vec<CompilerWarning>,
    pub file_name: String,
}

impl Program {
    pub fn new(file_name: String) -> Self {
        Self {
            body: None,
            warnings: Vec::new(),
            file_name,
        }
    }
}
