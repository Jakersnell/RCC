mod hlir;

use crate::parser::ast::ASTRoot;
use crate::util::str_intern::InternedStr;
use crate::util::CompilerResult;
use hlir::{BoundTypeKind, SymbolKind};
use std::collections::HashMap;

pub struct Validator<'a> {
    current_resolver: SymbolResolver<'a>,
    root: ASTRoot,
}

pub struct SymbolResolver<'a> {
    symbols: HashMap<InternedStr, SymbolKind>,
    parent: Option<&'a SymbolResolver<'a>>,
}

impl<'a> SymbolResolver<'a> {
    pub fn new(parent: Option<&'a SymbolResolver<'_>>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent,
        }
    }

    #[inline]
    pub fn ident_exists_in_scope(&self, ident: &InternedStr) -> bool {
        self.symbols.contains_key(ident)
    }

    #[inline]
    fn add_symbol(&mut self, ident: InternedStr, kind: SymbolKind) -> CompilerResult<()> {
        todo!()
    }

    #[inline]
    pub fn add_function(
        &mut self,
        ident: InternedStr,
        ty: BoundTypeKind,
        parameters: Vec<BoundTypeKind>,
    ) -> CompilerResult<()> {
        self.add_symbol(ident, SymbolKind::Function { ty, parameters })
    }

    #[inline]
    pub fn add_variable(&mut self, ident: InternedStr, ty: BoundTypeKind) -> CompilerResult<()> {
        self.add_symbol(ident, SymbolKind::Variable(ty))
    }
}

struct ControlFlowAnalyzer {
    // ...
}

struct ControlNode {
    // ...
}

struct ControlEdge {
    // ...
}

impl<'a> Validator<'a> {
    pub fn new(root: ASTRoot) -> Self {
        let root_resolver = SymbolResolver {
            symbols: HashMap::new(),
            parent: None,
        };
        Self {
            current_resolver: root_resolver,
            root,
        }
    }
}
