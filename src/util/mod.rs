use std::fmt::{Display, Formatter};
use std::io::Read;
use std::ops::Deref;

use derive_new::new;

use error::CompilerError;

use crate::data::tokens::Token as LexToken;

pub mod ast_pretty_print;
pub mod display_utils;
pub mod error;
pub mod mlir_display;
pub mod str_intern;

pub type LocatableToken = Locatable<LexToken>;
pub type CompilerResult<T> = Result<T, Vec<CompilerError>>;

#[derive(Debug, PartialEq, new, Hash, PartialOrd, Eq)]
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
    #[inline]
    pub fn merge_span(mut self, other: Span) -> Self {
        self.location.merge(other);
        self
    }

    #[inline]
    pub fn unwrap(self) -> T {
        self.value
    }
}

impl<T> Deref for Locatable<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Display for Locatable<T>
    where
        T: Display,
{
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Default, Hash, PartialOrd, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub col: usize,
    pub line: usize,
}

impl Span {
    #[inline]
    pub fn new(start: usize, end: usize, col: usize, line: usize) -> Self {
        debug_assert!(start < end);
        Self {
            start,
            end,
            col,
            line,
        }
    }

    #[inline]
    pub fn merge(mut self, other: Self) -> Self {
        self.end = other.end;
        self
    }

    #[inline]
    pub fn extend(mut self, other: Self) -> Self {
        self.end = other.start;
        self
    }

    #[inline]
    pub fn into_locatable<T>(self, value: T) -> Locatable<T> {
        Locatable::new(self, value)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "src: {}:{}:{}:{}",
            self.line, self.col, self.start, self.end
        )
    }
}
