use crate::ast::{Expression, InitDeclaration, Statement};
use crate::error::{CompilerError, CompilerWarning};
use crate::tokens::Token as LexToken;
use derive_new::new;

pub type LocatableToken = Locatable<LexToken>;
pub type CompilerResult<T> = Result<T, Vec<Locatable<CompilerError>>>;

#[derive(Debug, PartialEq, new)]
pub struct Locatable<T> {
    pub location: Span,
    pub value: T,
}

#[derive(Debug, PartialEq, new, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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
