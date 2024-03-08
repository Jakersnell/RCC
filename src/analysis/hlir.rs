use crate::analysis::hlir::HlirTypeKind::Double;
use crate::parser::ast::TypeSpecifier;
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
    pub fields: HashMap<InternedStr, HlirType>,
}

#[derive(Debug)]
pub struct HlirFunction {
    pub ty: HlirType,
    pub ident: InternedStr,
    pub parameters: HashMap<InternedStr, HlirType>,
    pub body: HlirBlock,
}

#[derive(Debug)]
pub struct HlirVariable {
    pub ty: HlirType,
    pub ident: InternedStr,
    pub is_array: bool,
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
    pub(crate) pointer: bool,
}
impl HlirType {
    pub fn can_implicitly_cast_to(&self, other: &HlirType) -> bool {
        if self == other {
            return true;
        }
        use HlirTypeKind::*;
        match (&self.kind, &other.kind, self.pointer, other.pointer) {
            (Char(_), Int(_), false, false) => true,
            (Char(_), Long(_), false, false) => true,
            (Char(_), Double, false, false) => true,
            (Int(_), Long(_), false, false) => true,
            (Int(_), LLong(_), false, false) => true,
            (Int(_), Double, false, false) => true,
            (Char(_), LLong(_), false, false) => true,
            (Long(_), LLong(_), false, false) => true,
            _ => false,
        }
    }

    pub fn can_explicitly_cast_to(&self, other: &HlirType) -> bool {
        if self.can_implicitly_cast_to(other) {
            return true;
        }
        use HlirTypeKind::*;
        match (&self.kind, &other.kind, self.pointer, other.pointer) {
            (Int(_), Char(_), false, false) => true,
            (Long(_), Char(_), false, false) => true,
            (LLong(_), Char(_), false, false) => true,
            (Double, Char(_), false, false) => true,
            (Long(_), Int(_), false, false) => true,
            (LLong(_), Int(_), false, false) => true,
            (Int(_), Double, false, false) => true,
            (Long(_), Double, false, false) => true,
            (LLong(_), Double, false, false) => true,
            (_, Long(_), true, false) => true, // all pointers can cast to long
            (_, LLong(_), true, false) => true, // all pointers can cast to long long
            _ => false,
        }
    }
}
impl Display for HlirType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.kind, if self.pointer { " *" } else { "" })
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

#[derive(Debug)]
pub enum HlirLiteral {
    Char(char),
    Integer(isize),
    Floating(f64),
}

#[derive(Debug, new)]
pub struct HlirExpr {
    kind: Box<HlirExprKind>,
    ty: HlirType,
    lval: bool,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
