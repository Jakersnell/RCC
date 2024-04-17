use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::data::mlir::{
    MlirExpr, MlirExprKind, MlirLiteral, MlirStruct, MlirType, MlirTypeDecl, MlirTypeKind,
    MlirVariable,
};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;
use crate::util::{str_intern, Locatable, Span};

pub type SymbolResult = Result<(), CompilerError>;
thread_local! {
    static BUILTINS: [(&'static str, FunctionSymbol); 3] = [
        (
            "printf",
            FunctionSymbol {
                location: Some("stdio.h"),
                params: vec![MlirType {
                    kind: MlirTypeKind::Char(true),
                    decl: MlirTypeDecl::Pointer,
                }],
                varargs: true,
                return_ty: MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Basic,
                },
            }
        ),
        (
            "malloc",
            FunctionSymbol {
                location: Some("stdlib.h"),
                  params: vec![MlirType {
                    kind: MlirTypeKind::Int(true),
                    decl: MlirTypeDecl::Basic,
                }],
                varargs: false,
                return_ty: MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Basic,
                },
            }
        ),
        (
            "free",
            FunctionSymbol {
                location: Some("stdlib.h"),
                params: vec![MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Pointer,
                }],
                varargs: false,
                return_ty: MlirType {
                    kind: MlirTypeKind::Void,
                    decl: MlirTypeDecl::Basic,
                },
            }
        ),
    ];
}

#[derive(Debug, Clone)]
pub(super) enum SymbolKind {
    Function(FunctionSymbol),
    Struct(StructSymbol),
    Variable(VariableSymbol),
}

#[derive(Debug, Clone)]
pub(super) struct StructSymbol {
    pub(super) size: u64,
    pub(super) as_type: MlirType,
    pub(super) body: HashMap<InternedStr, VariableSymbol>,
}

#[derive(Clone, Debug)]
pub(super) struct FunctionSymbol {
    pub(super) location: Option<&'static str>,
    pub(super) return_ty: MlirType,
    pub(super) varargs: bool,
    pub(super) params: Vec<MlirType>,
}

#[derive(Clone, Debug)]
pub(super) struct VariableSymbol {
    pub(super) ty: MlirType,
    pub(super) is_const: bool,
    pub(super) is_initialized: bool,
    pub(super) array_size: Option<u64>,
}

#[derive(Default, Debug)]
pub struct SymbolResolver {
    un_accessed_items: HashMap<InternedStr, Span>,
    pub(super) symbols: HashMap<InternedStr, SymbolKind>,
    pub(super) parent: Option<Box<RefCell<SymbolResolver>>>,
}

impl SymbolResolver {
    pub fn create_root() -> Self {
        let mut root = Self {
            un_accessed_items: HashMap::default(),
            symbols: HashMap::default(),
            parent: None,
        };
        root.init_builtins();
        root
    }

    pub fn remove_self(self) -> Option<Box<RefCell<Self>>> {
        self.parent
    }

    fn init_builtins(&mut self) {
        debug_assert!(self.parent.is_none());
        BUILTINS.with(|builtins| {
            for builtin in builtins.iter() {
                self.symbols.insert(
                    str_intern::intern(builtin.0),
                    SymbolKind::Function(builtin.1.clone()), // ignore access check on builtins
                );
            }
        });
    }
    pub fn new(parent: Option<Box<RefCell<SymbolResolver>>>) -> Self {
        Self {
            un_accessed_items: HashMap::default(),
            symbols: HashMap::default(),
            parent,
        }
    }

    #[inline]
    pub fn add_function(
        &mut self,
        ident: &InternedStr,
        return_ty: MlirType,
        params: Vec<MlirType>,
        span: Span,
    ) -> SymbolResult {
        let symbol = SymbolKind::Function(FunctionSymbol {
            location: None,
            return_ty,
            varargs: false,
            params,
        });
        self.add_symbol(ident, symbol, span)
    }

    #[inline]
    pub fn add_variable(
        &mut self,
        ident: &InternedStr,
        ty: &MlirType,
        is_const: bool,
        is_initialized: bool,
        array_size: Option<u64>,
        span: Span,
    ) -> SymbolResult {
        let symbol = SymbolKind::Variable(VariableSymbol {
            ty: ty.clone(),
            is_const,
            is_initialized,
            array_size,
        });
        self.add_symbol(ident, symbol, span)
    }

    #[inline]
    fn add_symbol(&mut self, ident: &InternedStr, kind: SymbolKind, span: Span) -> SymbolResult {
        if self.symbols.contains_key(ident) {
            Err(CompilerError::IdentifierExists(span))
        } else if !matches!(kind, SymbolKind::Function { .. }) && ident.as_ref() == "main" {
            Err(CompilerError::MainIsReserved(span))
        } else {
            self.symbols.insert(ident.clone(), kind);
            if ident.as_ref() != "main" {
                self.un_accessed_items.insert(ident.clone(), span);
            }
            Ok(())
        }
    }

    pub fn get_unused_idents(&self) -> Vec<(InternedStr, Span)> {
        self.un_accessed_items.clone().into_iter().collect()
    }

    pub fn validate_function_call(
        &mut self,
        ident: InternedStr,
        span: Span,
    ) -> Result<FunctionSymbol, CompilerError> {
        match self.retrieve(&ident, span)? {
            SymbolKind::Function(func) => Ok(func),
            _ => Err(CompilerError::NotAFunction(span)),
        }
    }

    pub fn check_valid_assignment(
        &mut self,
        ident: &InternedStr,
        ty: &MlirType,
        span: Span,
    ) -> SymbolResult {
        let var = match self.retrieve(ident, span)? {
            SymbolKind::Variable(var_ty) => Ok(var_ty),
            _ => Err(CompilerError::LeftHandNotLVal(span)),
        }?;

        if &var.ty != ty {
            Err(CompilerError::VariableTypeMismatch(
                span,
                ty.to_string(),
                var.ty.to_string(),
            ))
        } else if var.is_const {
            Err(CompilerError::ConstAssignment(span))
        } else {
            Ok(())
        }
    }

    pub fn get_variable_type(
        &mut self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<MlirType, CompilerError> {
        match self.retrieve(ident, span)? {
            SymbolKind::Variable(var) => Ok(var.ty.clone()),
            _ => Err(CompilerError::NotAVariable(span)),
        }
    }

    fn retrieve(&mut self, ident: &InternedStr, span: Span) -> Result<SymbolKind, CompilerError> {
        let symbol = self.symbols.get(ident);
        if let Some(symbol) = symbol {
            self.un_accessed_items.remove(ident);
            Ok(symbol.clone())
        } else if let Some(parent) = self.parent.as_ref() {
            parent.borrow_mut().retrieve(ident, span)
        } else {
            Err(CompilerError::IdentNotFound((*ident).clone(), span))
        }
    }

    pub fn add_struct(
        &mut self,
        as_type: MlirType,
        _struct: &MlirStruct,
        span: Span,
    ) -> SymbolResult {
        let ident = _struct.ident.clone();
        if cfg!(debug_assertions) {
            let ty_ident = match &as_type.kind {
                MlirTypeKind::Struct(ident) => ident,
                _ => panic!(),
            };
            assert_eq!(ident, *ty_ident);
        }
        let mut body = HashMap::default();
        for field in &_struct.fields {
            let array_size = if let MlirTypeDecl::Array(size) = &field.ty.decl {
                Some(*size)
            } else {
                None
            };
            let var = VariableSymbol {
                ty: field.ty.clone(),
                is_const: field.is_const,
                is_initialized: field.initializer.is_some(),
                array_size,
            };
            if body.contains_key(&ident) {
                return Err(CompilerError::MemberAlreadyExists(ident.clone(), span));
            }
            body.insert(field.ident.clone(), var);
        }
        let mut symbol = StructSymbol {
            size: _struct.size,
            as_type,
            body,
        };
        let symbol = SymbolKind::Struct(symbol);
        self.add_symbol(&ident, symbol, span)
    }

    fn get_struct(
        &mut self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<StructSymbol, CompilerError> {
        match self.retrieve(ident, span)? {
            SymbolKind::Struct(s) => Ok(s),
            _ => Err(CompilerError::NotAStruct(span)),
        }
    }
    pub fn validate_struct_member_access(
        &mut self,
        _struct: Locatable<MlirExpr>,
        member: Locatable<InternedStr>,
    ) -> Result<MlirExpr, CompilerError> {
        let ident = match &_struct.ty.kind {
            MlirTypeKind::Struct(ident) => ident,
            _ => panic!("`validate_struct_member_access` called on non struct expression."),
        };
        let _match = self.get_struct(ident, _struct.location)?;
        let ty = _match
            .body
            .get(&member.value)
            .ok_or(CompilerError::MemberNotFound(
                member.value.to_string(),
                ident.to_string(),
                member.location,
            ));
        let ty = ty?.ty.clone();
        Ok(MlirExpr {
            span: _struct.location.merge(member.location),
            kind: Box::new(MlirExprKind::Member(_struct.value, member.value)),
            is_lval: true,
            ty,
        })
    }

    pub fn get_struct_size(
        &mut self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<u64, CompilerError> {
        Ok(self.get_struct(ident, span)?.size)
    }

    pub fn check_struct_exists(
        &mut self,
        ident: &InternedStr,
        span: Span,
    ) -> Result<(), CompilerError> {
        self.get_struct(ident, span).map(|_| ())
    }
}

#[test]
fn test_builtin_function_is_called_correctly() {
    let mut resolver = crate::analysis::symbols::SymbolResolver::create_root();
    let ident = crate::util::str_intern::intern("printf");
    let call = resolver.validate_function_call(ident, Span::default());
    assert!(call.is_ok())
}

#[test]
fn test_parent_scope_is_accessed_in_retrieve() {
    let mut resolver = SymbolResolver::create_root();
    let symbol = SymbolKind::Struct(StructSymbol {
        size: 0,
        as_type: MlirType {
            kind: MlirTypeKind::Void,
            decl: MlirTypeDecl::Basic,
        },
        body: Default::default(),
    });
    let ident = "test_ident".into();
    resolver.add_symbol(&ident, symbol, Span::default());
    resolver = SymbolResolver::new(Some(Box::new(RefCell::new(resolver))));
    let result = resolver.retrieve(&ident, Span::default());
    assert!(result.is_ok());
}
