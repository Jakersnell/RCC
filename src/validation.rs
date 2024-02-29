use crate::ast::ASTRoot;
use crate::error::CompilerError;
use crate::hlir::{BoundTypeKind, SymbolKind};
use crate::str_intern::InternedStr;
use crate::util::{CompilerResult, Locatable, Span};
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
        if self.ident_exists_in_scope(&ident) {
            Err(vec![Locatable::new(
                Span::new(0, 0, 0, 0), // TODO: replace with idents location
                CompilerError::IdentifierExists(ident.to_string()),
            )])
        } else {
            self.symbols.insert(ident, kind);
            Ok(())
        }
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
