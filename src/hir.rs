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

struct Validator<'a> {
    root_resolver: SymbolResolver<'a>,
    current_resolver: SymbolResolver<'a>,
}

pub enum BoundType {
    UnsignedInt(u32),
    Int(i32),
    UnsignedLong(u64),
    Long(i64),
    Float(f32),
    Double(f64),
}

pub enum BoundLiteral {
    Int(i32),
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
