use crate::data::mlir::{MlirExpr, MlirExprKind, MlirLiteral};

// }

// pub enum MlirLiteral {
//     Int(i64),
//     UInt(u64),
//     Float(f64),
//     String(Vec<u8>),
//     Char(u8),

macro_rules! binary_fold {
    ($a:expr, $op:tt, $b:expr) => {{
        use MlirLiteral::*;
        match (get_constant($a), get_constant($b)) {
            (Char(a), Char(b)) => Char(a $op b),
            (Int(a), Int(b)) => Int(a $op b),
            (UInt(a), UInt(b)) => UInt(a $op b),
            (Float(a), Float(b)) => Float(a $op b),
            (a, b) => panic!("Cannot use types:\n{:#?}\n{:#?}", a, b)
        }
    }};
}

pub fn fold_binary_expression(expr: MlirExpr) -> MlirExpr {
    let MlirExpr {
        span,
        kind,
        ty,
        is_lval,
    } = expr;
    debug_assert!(!is_lval);
    match *kind {
        MlirExprKind::Add(left, right) => {
            binary_fold!(left, +, right);
        }
        MlirExprKind::Sub(left, right) => {}
        MlirExprKind::Mul(left, right) => {}
        MlirExprKind::Div(left, right) => {}
        MlirExprKind::Mod(left, right) => {}
        MlirExprKind::Equal(left, right) => {}
        MlirExprKind::NotEqual(left, right) => {}
        MlirExprKind::GreaterThan(left, right) => {}
        MlirExprKind::GreaterThanEqual(left, right) => {}
        MlirExprKind::LessThan(left, right) => {}
        MlirExprKind::LessThanEqual(left, right) => {}
        MlirExprKind::LogicalAnd(left, right) => {}
        MlirExprKind::LogicalOr(left, right) => {}
        MlirExprKind::BitwiseAnd(left, right) => {}
        MlirExprKind::BitwiseOr(left, right) => {}
        MlirExprKind::BitwiseXor(left, right) => {}
        MlirExprKind::LeftShift(left, right) => {}
        MlirExprKind::RightShift(left, right) => {}
        _ => panic!(
            "Unsupported mlir expression given for binary folding: {:#?}",
            kind
        ),
    }
    todo!()
}

fn get_constant(expr: MlirExpr) -> MlirLiteral {
    match *expr.kind {
        MlirExprKind::Literal(literal) => literal,
        _ => panic!("Attempted access of non const as const."),
    }
}
