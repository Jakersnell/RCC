use crate::ast::{Declaration, DeclarationType, FunctionDeclaration};
use crate::str_intern::InternedStr;
use crate::util::Program;
use std::collections::HashMap;

struct SymbolResolver<'a> {
    symbols: HashMap<InternedStr, SymbolType<'a>>,
    parent: Option<&'a SymbolResolver<'a>>,
}

enum SymbolType<'a> {
    Function(&'a FunctionDeclaration),
    Variable(&'a Declaration),
}
