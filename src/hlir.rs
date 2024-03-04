use crate::ast::{ASTRoot, Declaration, FunctionDeclaration};
use crate::str_intern::InternedStr;
use std::collections::HashMap;

pub struct BoundRoot(pub Vec<BoundStatement>);

pub enum BoundInitDeclaration {
    VariableDeclaration {
        ident: InternedStr,
        ty: BoundTypeKind,
        initializer: Option<BoundExpr>,
    },
    FunctionDeclaration {
        ident: InternedStr,
        ty: BoundTypeKind,
        parameters: Vec<(InternedStr, BoundTypeKind)>,
        body: Vec<BoundStatement>,
    },
}

pub enum BoundStatement {
    VariableDeclaration {
        ident: InternedStr,
        ty: BoundTypeKind,
        initializer: Option<BoundExpr>,
    },
    FunctionDeclaration {
        ident: InternedStr,
        ty: BoundTypeKind,
        parameters: Vec<(InternedStr, BoundTypeKind)>,
        body: Vec<BoundStatement>,
    },
    Return(BoundExpr),
    Expression(BoundExpr),
}

#[derive(Debug)]
pub enum SymbolKind {
    Function {
        ty: BoundTypeKind,
        parameters: Vec<BoundTypeKind>,
    },
    Variable(BoundTypeKind),
}

#[derive(Debug)]
pub enum BoundTypeKind {
    Void,
    Int(bool), // signed/unsigned
    Long(bool),
    Float,
    Double,
}

#[derive(Debug)]
pub enum BoundLiteral {
    Int(u32),
    Float(f32),
    Double(f64),
}

#[derive(Debug)]
pub struct BoundExpr {
    pub ty: BoundTypeKind,
    pub kind: BoundExprKind,
}

#[derive(Debug)]
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
