pub mod arch;
pub mod ast_pretty_print;
pub mod error;
pub mod str_intern;

use crate::lexer::tokens::Token as LexToken;
use crate::parser::ast::InitDeclaration;
use crate::util::error::CompilerWarning;
use arcstr::ArcStr;
use derive_new::new;
use error::CompilerError;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::io::Read;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

pub type LocatableToken = Locatable<LexToken>;
pub type CompilerResult<T> = Result<T, Vec<CompilerError>>;

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

#[derive(Debug, PartialEq, new, Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub col: usize,
    pub line: usize,
}

impl Span {
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
            "src: {}:{}:{}",
            self.line,
            self.col,
            self.end - self.start
        )
    }
}
