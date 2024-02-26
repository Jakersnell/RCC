use crate::ast::{ASTRoot, Declaration, DeclarationType, FunctionDeclaration};
use crate::str_intern::InternedStr;
use crate::util::Program;
use std::collections::HashMap;

struct Validator<'a> {
    root_resolver: SymbolResolver<'a>,
    current_resolver: SymbolResolver<'a>,
    root: ASTRoot,
}

struct SymbolResolver<'a> {
    symbols: HashMap<InternedStr, SymbolKind>,
    parent: Option<&'a SymbolResolver<'a>>,
}

enum SymbolKind {
    Function {
        ty: BoundType,
        parameters: Vec<BoundType>,
    },
    Variable(BoundType),
}

pub enum BoundType {
    Void,
    Int(bool), // signed/unsigned
    Long(bool),
    Float,
    Double,
}

pub enum BoundLiteral {
    Int(u32),
    Float(f32),
    Double(f64),
}

pub struct BoundExpr {
    pub ty: BoundType,
    pub kind: BoundExprKind,
}

pub enum BoundExprKind {
    Literal(BoundLiteral),
    Variable(Declaration),
    FunctionCall(InternedStr, Vec<BoundExpr>),
    Assignment {
        target: Box<BoundExpr>,
        value: Box<BoundExpr>,
    },
    BinaryOperation {
        left: Box<BoundExpr>,
        right: Box<BoundExpr>,
        op: crate::ast::BinaryOp,
    },
    UnaryOperation {
        operand: Box<BoundExpr>,
        op: crate::ast::UnaryOp,
    },
}
