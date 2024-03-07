mod flow;
pub mod hlir;
mod symbols;

use crate::analysis::hlir::HighLevelIR;
use crate::analysis::symbols::SymbolResolver;
use crate::parser::ast::{ASTRoot, AbstractSyntaxTree};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;
use crate::util::Locatable;
use std::collections::HashMap;

pub struct Validator<'a> {
    hlir: HighLevelIR,
    global: SymbolResolver<'a>,
    ast: AbstractSyntaxTree,
}

impl<'a> Validator<'a> {
    pub fn new(ast: AbstractSyntaxTree) -> Self {
        let global = SymbolResolver::create_root();
        let hlir = HighLevelIR::default();
        Self { hlir, global, ast }
    }
}
