use crate::analysis::hlir::HlirTypeDecl::{Basic, Pointer};
use crate::analysis::hlir::HlirTypeKind::{Double, Float};
use crate::parser::ast::{BinaryOp, Block, TypeSpecifier};
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
    pub globals: HashMap<InternedStr, HlirVariable>,
}

#[derive(Debug)]
pub struct HlirStruct {
    pub ident: InternedStr,
    pub fields: Vec<HlirVariable>,
    pub size: u64,
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
        matches!(self.decl, HlirTypeDecl::Array(_)) && !self.is_pointer()
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.decl, HlirTypeDecl::Pointer)
    }

    pub fn is_numeric(&self) -> bool {
        use HlirTypeKind::*;
        matches!(&self.kind, Char(_) | Int(_) | Long(_) | Float | Double) && !self.is_pointer()
    }

    pub fn is_integer(&self) -> bool {
        use HlirTypeKind::*;
        matches!(&self.kind, Char(_) | Int(_) | Long(_)) && !self.is_pointer()
    }
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
            (Int(_), Double, Basic, Basic) => Some(Double),
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
            (Double, Char(unsigned), Basic, Basic) => Some(Char(*unsigned)),
            (Long(_), Int(unsigned), Basic, Basic) => Some(Int(*unsigned)),
            (Int(_), Double, Basic, Basic) => Some(Double),
            (Long(_), Double, Basic, Basic) => Some(Double),
            (_, Long(unsigned), Pointer, Basic) => Some(Long(*unsigned)), // all pointers can cast to long
            _ => None,
        };

        ty_kind.map(|kind| HlirType::new(kind, Basic))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HlirTypeDecl {
    Basic,
    Pointer, // true if pointer is const
    Array(u64),
}

impl Display for HlirTypeDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use HlirTypeDecl::*;
        match self {
            Basic => Ok(()),
            Pointer => write!(f, " *"),
            Array(size) => write!(f, " [{}]", size),
        }
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
    Char(bool), // 8
    Int(bool),  // signed/unsigned
    Long(bool), // i64
    Float,
    Double,
    Struct(InternedStr),
}

impl Display for HlirTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_signed {
            ($name:literal, $unsigned:expr) => {
                write!(
                    f,
                    "{} {}",
                    if $unsigned { "unsigned" } else { "signed" },
                    $name
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum HlirLiteral {
    Int(i64),
    UInt(u64),
    Float(f64),
    String(Vec<u8>),
    Char(u8),
}

#[derive(Debug, Clone, new)]
pub struct HlirExpr {
    pub kind: Box<HlirExprKind>,
    pub ty: HlirType,
    pub is_lval: bool,
}

impl HlirExpr {
    pub fn is_literal(&self) -> bool {
        matches!(self.kind.as_ref(), HlirExprKind::Literal(_))
    }
    pub fn is_string(&self) -> bool {
        matches!(
            self.kind.as_ref(),
            HlirExprKind::Literal(HlirLiteral::String(_))
        )
    }
    pub fn is_integer_or_pointer(&self) -> bool {
        self.ty.is_integer() || self.ty.is_pointer()
    }

    pub fn is_integer(&self) -> bool {
        self.ty.is_integer()
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
    PostIncrement(HlirExpr),
    PostDecrement(HlirExpr),
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
    FunctionCall {
        location: Option<&'static str>,
        ident: InternedStr,
        args: Vec<HlirExpr>,
    },
    Index(HlirExpr, HlirExpr),
    Member(HlirExpr, InternedStr), // offset
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
    Block(HlirBlock),
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
    Return(Option<HlirExpr>),
}
