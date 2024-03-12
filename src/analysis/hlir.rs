use crate::analysis::hlir::HlirTypeKind::{Double, Float};
use crate::parser::ast::{BinaryOp, TypeSpecifier};
use crate::util::error::CompilerError;
use crate::util::str_intern::InternedStr;
use crate::util::Locatable;
use arcstr::ArcStr;
use derive_new::new;
use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::{Mutex, MutexGuard, OnceLock};

#[derive(Debug, Default)]
pub struct HighLevelIR {
    pub functions: HashMap<InternedStr, HlirFunction>,
    pub structs: HashMap<InternedStr, HlirStruct>,
    pub globals: Vec<HlirVariable>,
}

#[derive(Debug)]
pub struct HlirStruct {
    pub ident: InternedStr,
    pub fields: Vec<HlirVariable>,
    pub size: usize,
}

#[derive(Debug)]
pub struct HlirFunction {
    pub ty: HlirType,
    pub ident: InternedStr,
    pub parameters: Vec<HlirVariable>,
    pub body: HlirBlock,
}

#[derive(Debug)]
pub struct HlirVariable {
    pub ty: HlirType,
    pub ident: InternedStr,
    pub array: Option<usize>,
    pub is_const: bool,
    pub initializer: Option<HlirVarInit>,
}

#[derive(Debug)]
pub enum HlirVarInit {
    Expr(HlirExpr),
    Array(Vec<HlirExpr>),
}

#[derive(Debug, Clone, PartialEq, new)]
pub struct HlirType {
    pub(crate) kind: HlirTypeKind,
    pub(crate) decl: HlirTypeDecl,
}

impl HlirType {
    pub fn is_array(&self) -> bool {
        matches!(self.decl, HlirTypeDecl::Array)
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.decl, HlirTypeDecl::Pointer(_))
    }

    pub fn is_numeric(&self) -> bool {
        self.kind.is_numeric()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HlirTypeDecl {
    Basic,
    Pointer(bool), // true if pointer is const
    Array,         // TODO: support array size
}

impl Display for HlirTypeDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use HlirTypeDecl::*;
        match self {
            Basic => Ok(()),
            Pointer(constant) => write!(f, "*{}", if *constant { "const " } else { "" }),
            Array => write!(f, "[]"),
        }
    }
}

impl HlirType {
    pub fn try_implicit_cast(&self, to: &HlirType) -> Option<HlirType> {
        if self == to {
            return None;
        }
        use HlirTypeDecl::*;
        use HlirTypeKind::*;
        let ty_kind = match (&self.kind, &to.kind, &self.decl, &to.decl) {
            (Char(_), Int(unsigned), Basic, Basic) => Some(Int(*unsigned)),
            (Char(_), Long(unsigned), Basic, Basic) => Some(Long(*unsigned)),
            (Char(_), Double, Basic, Basic) => Some(Double),
            (Int(_), Long(unsigned), Basic, Basic) => Some(Long(*unsigned)),
            (Int(_), LLong(unsigned), Basic, Basic) => Some(LLong(*unsigned)),
            (Int(_), Double, Basic, Basic) => Some(Double),
            (Char(_), LLong(unsigned), Basic, Basic) => Some(LLong(*unsigned)),
            (Long(_), LLong(unsigned), Basic, Basic) => Some(LLong(*unsigned)),
            _ => None,
        };
        ty_kind.map(|kind| HlirType::new(kind, Basic))
    }
    pub fn try_explicit_cast(&self, other: &HlirType) -> Option<HlirType> {
        let implicit = self.try_implicit_cast(other);
        if implicit.is_some() {
            return implicit;
        }

        use HlirTypeDecl::*;
        use HlirTypeKind::*;
        let ty_kind = match (&self.kind, &other.kind, &self.decl, &other.decl) {
            (Int(_), Char(unsigned), Basic, Basic) => Some(Char(*unsigned)),
            (Long(_), Char(unsigned), Basic, Basic) => Some(Char(*unsigned)),
            (LLong(_), Char(unsigned), Basic, Basic) => Some(Char(*unsigned)),
            (Double, Char(unsigned), Basic, Basic) => Some(Char(*unsigned)),
            (Long(_), Int(unsigned), Basic, Basic) => Some(Int(*unsigned)),
            (LLong(_), Int(unsigned), Basic, Basic) => Some(Int(*unsigned)),
            (Int(_), Double, Basic, Basic) => Some(Double),
            (Long(_), Double, Basic, Basic) => Some(Double),
            (LLong(_), Double, Basic, Basic) => Some(Double),
            (_, Long(unsigned), Pointer(_), Basic) => Some(Long(*unsigned)), // all pointers can cast to long
            (_, LLong(unsigned), Pointer(_), Basic) => Some(LLong(*unsigned)), // all pointers can cast to long long
            _ => None,
        };

        ty_kind.map(|kind| HlirType::new(kind, Basic))
    }
}
impl Display for HlirType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.kind, self.decl)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HlirTypeKind {
    Void,
    Char(bool),  // 8
    Int(bool),   // signed/unsigned
    Long(bool),  // i64
    LLong(bool), // i128
    Float,
    Double,
    Struct(InternedStr),
}

impl HlirTypeKind {
    pub fn is_numeric(&self) -> bool {
        use HlirTypeKind::*;
        matches!(self, Char(_) | Int(_) | Long(_) | LLong(_) | Float | Double)
    }

    pub fn is_integer(&self) -> bool {
        use HlirTypeKind::*;
        matches!(self, Char(_) | Int(_) | Long(_) | LLong(_))
    }
}

impl Display for HlirTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_signed {
            ($name:literal, $signed:expr) => {
                write!(
                    f,
                    concat!($name, " {}"),
                    if $signed { "signed" } else { "unsigned" }
                )
            };
        }
        use HlirTypeKind::*;
        match self {
            Void => write!(f, "void"),
            Char(unsigned) => write_signed!("char", *unsigned),
            Int(unsigned) => write_signed!("int", *unsigned),
            Long(unsigned) => write_signed!("long", *unsigned),
            Float => write!(f, "float"),
            Double => write!(f, "double"),
            Struct(ident) => write!(f, "struct {}", ident),
            LLong(unsigned) => write_signed!("long long", *unsigned),
        }
    }
}

#[derive(Debug, Clone)]
pub enum HlirLiteral {
    Integer(isize),
    Floating(f64),
}

#[derive(Debug, Clone, new)]
pub struct HlirExpr {
    pub kind: Box<HlirExprKind>,
    pub ty: HlirType,
    pub is_lval: bool,
}

impl HlirExpr {
    pub fn is_integer_or_pointer(&self) -> bool {
        self.ty.kind.is_integer() || self.ty.is_pointer()
    }

    pub fn is_integer(&self) -> bool {
        self.ty.kind.is_integer()
    }

    pub fn is_pointer(&self) -> bool {
        self.ty.is_pointer()
    }

    pub fn is_array(&self) -> bool {
        self.ty.is_array()
    }

    pub fn is_numeric(&self) -> bool {
        self.ty.is_numeric()
    }
}

#[derive(Debug, Clone)]
pub enum HlirExprKind {
    Literal(HlirLiteral),
    Variable(InternedStr),

    // unary
    Increment(HlirExpr, bool), // post/pre
    Decrement(HlirExpr, bool),
    Negate(HlirExpr),
    LogicalNot(HlirExpr),
    BitwiseNot(HlirExpr),
    Deref(HlirExpr),
    AddressOf(HlirExpr),

    // binary
    Add(HlirExpr, HlirExpr),
    Sub(HlirExpr, HlirExpr),
    Mul(HlirExpr, HlirExpr),
    Div(HlirExpr, HlirExpr),
    Mod(HlirExpr, HlirExpr),
    Equal(HlirExpr, HlirExpr),
    NotEqual(HlirExpr, HlirExpr),
    GreaterThan(HlirExpr, HlirExpr),
    GreaterThanEqual(HlirExpr, HlirExpr),
    LessThan(HlirExpr, HlirExpr),
    LessThanEqual(HlirExpr, HlirExpr),
    LogicalAnd(HlirExpr, HlirExpr),
    LogicalOr(HlirExpr, HlirExpr),
    BitwiseAnd(HlirExpr, HlirExpr),
    BitwiseOr(HlirExpr, HlirExpr),
    BitwiseXor(HlirExpr, HlirExpr),
    LeftShift(HlirExpr, HlirExpr),
    RightShift(HlirExpr, HlirExpr),

    // assign
    Assign(HlirExpr, HlirExpr),
    AssignAdd(HlirExpr, HlirExpr),
    AssignSub(HlirExpr, HlirExpr),
    AssignMul(HlirExpr, HlirExpr),
    AssignMod(HlirExpr, HlirExpr),
    AssignBitAnd(HlirExpr, HlirExpr),
    AssignBitOr(HlirExpr, HlirExpr),
    AssignBitXor(HlirExpr, HlirExpr),
    AssignLeftShift(HlirExpr, HlirExpr),
    AssignRightShift(HlirExpr, HlirExpr),

    // other
    FunctionCall(CallType),
    Index(HlirExpr, HlirExpr),
    Member(HlirExpr, InternedStr),
    Cast(HlirType, HlirExpr),
}

impl HlirExprKind {
    pub fn is_literal(&self) -> bool {
        matches!(self, HlirExprKind::Literal(_))
    }
    pub fn try_from_binop(op: BinaryOp, left: HlirExpr, right: HlirExpr) -> Result<Self, ()> {
        match op {
            BinaryOp::Add => Ok(HlirExprKind::Add(left, right)),
            BinaryOp::Sub => Ok(HlirExprKind::Sub(left, right)),
            BinaryOp::Mul => Ok(HlirExprKind::Mul(left, right)),
            BinaryOp::Div => Ok(HlirExprKind::Div(left, right)),
            BinaryOp::Mod => Ok(HlirExprKind::Mod(left, right)),
            BinaryOp::Equal => Ok(HlirExprKind::Equal(left, right)),
            BinaryOp::NotEqual => Ok(HlirExprKind::NotEqual(left, right)),
            BinaryOp::GreaterThan => Ok(HlirExprKind::GreaterThan(left, right)),
            BinaryOp::GreaterThanEqual => Ok(HlirExprKind::GreaterThanEqual(left, right)),
            BinaryOp::LessThan => Ok(HlirExprKind::LessThan(left, right)),
            BinaryOp::LessThanEqual => Ok(HlirExprKind::LessThanEqual(left, right)),
            BinaryOp::LogicalAnd => Ok(HlirExprKind::LogicalAnd(left, right)),
            BinaryOp::LogicalOr => Ok(HlirExprKind::LogicalOr(left, right)),
            BinaryOp::BitwiseAnd => Ok(HlirExprKind::BitwiseAnd(left, right)),
            BinaryOp::BitwiseOr => Ok(HlirExprKind::BitwiseOr(left, right)),
            BinaryOp::BitwiseXor => Ok(HlirExprKind::BitwiseXor(left, right)),
            BinaryOp::LeftShift => Ok(HlirExprKind::LeftShift(left, right)),
            BinaryOp::RightShift => Ok(HlirExprKind::RightShift(left, right)),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CallType {
    FunctionCall {
        ident: InternedStr,
        args: Vec<HlirExpr>,
    },
    BuiltinCall {
        location: &'static str,
        ident: &'static str,
        args: Vec<HlirExpr>,
    },
}

#[derive(Debug)]
pub struct HlirBlock(pub Vec<HlirStmt>);
impl Deref for HlirBlock {
    type Target = Vec<HlirStmt>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub enum HlirStmt {
    Expression(HlirExpr),
    VariableDeclaration(HlirVariable),
    If {
        condition: HlirExpr,
        then: Box<HlirStmt>,
        otherwise: Option<Box<HlirStmt>>,
    },
    While {
        condition: HlirExpr,
        body: Box<HlirStmt>,
    },
    Continue,
    Break,
    Return(HlirExpr),
}
