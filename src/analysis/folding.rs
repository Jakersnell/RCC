use crate::data::mlir::{MlirExpr, MlirExprKind, MlirLiteral};

macro_rules! binary_fold {
    ($a:expr, $op:tt, $b:expr) => {{
        use MlirLiteral::*;
        match (get_constant($a), get_constant($b)) {
            (UChar(a), UChar(b)) => UChar(a $op b),
            (UChar(a), Long(b)) => Long(a as i64 $op b),
            (UChar(a), ULong(b)) => ULong(a as u64 $op b),
            (UChar(a), Double(b)) => Double(a as f64 $op b),

            (Long(a), UChar(b)) => Long(a $op b as i64),
            (Long(a), Long(b)) => Long(a $op b),
            (Long(a), ULong(b)) => Long(a $op b as i64),
            (Long(a), Double(b)) => Double(a as f64 $op b),

            (ULong(a), UChar(b)) => ULong(a $op b as u64),
            (ULong(a), Long(b)) => Long(a as i64 $op b),
            (ULong(a), ULong(b)) => ULong(a $op b),
            (ULong(a), Double(b)) => Double(a as f64 $op b),

            (ULong(a), ULong(b)) => ULong(a $op b),
            (Double(a), Double(b)) => Double(a $op b),
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
