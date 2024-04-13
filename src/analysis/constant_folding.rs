use crate::data::mlir::*;

macro_rules! bin_fold_literal {
    ($mac:ident, $left:expr, $op:tt, $right:expr, $($lit_kind:ident),+) => {
        match ($left.get_const(), $right.get_const()) {
            $(
                 (MlirLiteral::$lit_kind(left), MlirLiteral::$lit_kind(right)) => $mac!(left, $op, right, $lit_kind),
            )+
            // a panic occuring here indicates an issue in the casting system
            (left, right) => panic!("'{:?}' {} '{:?} is not a supported operation.", left, stringify!($op), right)
        }
    }
}

macro_rules! bin_fold {
    ($span:expr, $ty:expr, $kind:ident, $l:expr, $op:tt, $r:expr, $folding_mac:ident, $mac:ident, $($lit_kind:ident),+) => {{
        let (l, r) = ($l.fold(), $r.fold());
        let expr_kind = if l.is_const() && r.is_const() {
            MlirExprKind::Literal($folding_mac!($mac, l, $op, r, $($lit_kind),+))
        } else {
            MlirExprKind::$kind(l, r)
        };
        MlirExpr {
            kind: Box::new(expr_kind),
            is_lval: false,
            span: $span,
            ty: $ty,
        }
    }};
}

impl MlirExpr {
    pub(in crate::analysis) fn fold(self) -> Self {
        match &*self.kind {
            MlirExprKind::Assign(_, _)
            | MlirExprKind::FunctionCall { .. }
            | MlirExprKind::Index(_, _)
            | MlirExprKind::Member(_, _)
            | MlirExprKind::Deref(_)
            | MlirExprKind::Literal(_)
            | MlirExprKind::Variable(_)
            | MlirExprKind::PostIncrement(_)
            | MlirExprKind::PostDecrement(_)
            | MlirExprKind::AddressOf(_) => self,

            MlirExprKind::Add(_, _)
            | MlirExprKind::Sub(_, _)
            | MlirExprKind::Mul(_, _)
            | MlirExprKind::Div(_, _)
            | MlirExprKind::Mod(_, _) => self.arithmetic_fold(),

            MlirExprKind::Equal(_, _)
            | MlirExprKind::NotEqual(_, _)
            | MlirExprKind::GreaterThan(_, _)
            | MlirExprKind::GreaterThanEqual(_, _)
            | MlirExprKind::LessThan(_, _)
            | MlirExprKind::LessThanEqual(_, _) => self.equivalence_fold(),

            MlirExprKind::BitwiseAnd(_, _)
            | MlirExprKind::BitwiseOr(_, _)
            | MlirExprKind::BitwiseXor(_, _)
            | MlirExprKind::LeftShift(_, _)
            | MlirExprKind::RightShift(_, _) => self.bitwise_fold(),

            MlirExprKind::LogicalAnd(_, _) | MlirExprKind::LogicalOr(_, _) => todo!(),

            MlirExprKind::Negate(_) | MlirExprKind::LogicalNot(_) | MlirExprKind::BitwiseNot(_) => {
                self.fold_unary()
            }

            MlirExprKind::Cast(_, _) => self.fold_cast(),
        }
    }

    fn arithmetic_fold(self) -> Self {
        let MlirExpr {
            span,
            ty,
            is_lval,
            kind,
        } = self;

        macro_rules! fold_arithmetic_literal {
            ($left:expr, $op:tt, $right:expr, $lit_kind:ident) => {
                MlirLiteral::$lit_kind($left $op $right)
            }
        }

        macro_rules! fold {
            ($kind:ident, $l:expr, $op:tt, $r:expr) => {{
                bin_fold!(
                    span,
                    ty,
                    $kind,
                    $l,
                    $op,
                    $r,
                    fold_literal,
                    fold_arithmetic_literal,
                    Char,
                    UChar,
                    Int,
                    UInt,
                    Long,
                    ULong,
                    Float,
                    Double
                )
            }};
        }

        match *kind {
            MlirExprKind::Add(left, right) => fold!(Add, left, +, right),
            MlirExprKind::Sub(left, right) => fold!(Sub, left, -, right),
            MlirExprKind::Mul(left, right) => fold!(Mul, left, *, right),
            MlirExprKind::Div(left, right) => fold!(Div, left, /, right),
            MlirExprKind::Mod(left, right) => fold!(Mod, left, %, right),
            unsupported => panic!("Cannot perform arithmetic fold on type: {:?}", unsupported),
        }
    }

    fn equivalence_fold(self) -> Self {
        let MlirExpr {
            span,
            ty,
            is_lval,
            kind,
        } = self;

        macro_rules! fold_eq_literal {
            ($left:expr, $op:tt, $right:expr, $lit_kind:ident) =>{
                MlirLiteral::Int(if $right $op $left {1} else {0})
            };
        }

        macro_rules! fold_eq {
            ($kind:ident, $left:expr, $op:tt, $right:expr) => {
                bin_fold!(
                    span,
                    MlirType {
                        kind: MlirTypeKind::Int(false),
                        decl: MlirTypeDecl::Basic
                    },
                    $kind,
                    $left,
                    $op,
                    $right,
                    fold_literal,
                    fold_eq_literal,
                    Char,
                    UChar,
                    Int,
                    UInt,
                    Long,
                    ULong,
                    Float,
                    Double
                )
            };
        }

        match *kind {
            MlirExprKind::Equal(left, right) => fold_eq!(Equal, left, ==, right),
            MlirExprKind::NotEqual(left, right) => fold_eq!(NotEqual, left, !=, right),
            MlirExprKind::GreaterThan(left, right) => fold_eq!(GreaterThan, left, >, right),
            MlirExprKind::GreaterThanEqual(left, right) => {
                fold_eq!(GreaterThanEqual, left, >=, right)
            }
            MlirExprKind::LessThan(left, right) => fold_eq!(LessThan, left, <, right),
            MlirExprKind::LessThanEqual(left, right) => fold_eq!(LessThanEqual, left, <=, right),
            kind => panic!("Cannot perform equivalence fold on type '{:?}'", kind),
        }
    }

    fn bitwise_fold(self) -> Self {
        let MlirExpr { span, ty, kind, .. } = self;

        macro_rules! fold_bit_literal {
            ($left:expr, $op:tt, $right:expr, $lit_kind:ident) =>{
                MlirLiteral::$lit_kind($right $op $left)
            };
        }

        macro_rules! fold_bit {
            ($kind:ident, $left:expr, $op:tt, $right:expr) => {
                bin_fold!(
                    span,
                    ty,
                    $kind,
                    $left,
                    $op,
                    $right,
                    fold_literal,
                    fold_bit_literal,
                    Char,
                    UChar,
                    Int,
                    UInt,
                    Long,
                    ULong
                )
            };
        }

        match *kind {
            MlirExprKind::BitwiseAnd(left, right) => fold_bit!(BitwiseAnd, left, &, right),
            MlirExprKind::BitwiseOr(left, right) => fold_bit!(BitwiseOr, left, |, right),
            MlirExprKind::BitwiseXor(left, right) => fold_bit!(BitwiseXor, left, ^, right),
            MlirExprKind::LeftShift(left, right) => fold_bit!(LeftShift, left, <<, right),
            MlirExprKind::RightShift(left, right) => fold_bit!(RightShift, left, >>, right),
            kind => panic!("Cannot perform bitwise fold on type '{:?}'", kind),
        }
    }

    fn logical_fold(self) -> Self {
        let MlirExpr { span, ty, kind, .. } = self;

        macro_rules! fold_logical_literal {
            ($mac:ident, $left:expr, $op:tt, $right:expr, $($lit_kind:ident),+) => {
                match ($left.get_const(), $right.get_const()) {
                    (MlirLiteral::Double(left), MlirLiteral::Double(right)) => MlirLiteral::Int(if left != 0.0 $op right != 0.0 {1} else {0}),
                    (MlirLiteral::Float(left), MlirLiteral::Float(right)) => MlirLiteral::Int(if left != 0.0 $op right != 0.0 {1} else {0}),
                    $(
                         (MlirLiteral::$lit_kind(left), MlirLiteral::$lit_kind(right)) => MlirLiteral::Int(if left != 0 $op right != 0 {1} else {0}),
                    )+
                    (left, right) => panic!("'{:?}' {} '{:?} is not a supported operation.", left, stringify!($op), right)
                }
            }
        }

        macro_rules! fold_logical {
            ($kind:ident, $left:expr, $op:tt, $right:expr) => {
                bin_fold!(
                    span,
                    ty,
                    $kind,
                    $left,
                    $op,
                    $right,
                    fold_logical_literal,
                    fold_logical_literal,
                    Char,
                    UChar,
                    Int,
                    UInt,
                    Long,
                    ULong
                )
            };
        }

        match *kind {
            MlirExprKind::LogicalAnd(left, right) => fold_logical!(LogicalAnd, left, &&, right),
            MlirExprKind::LogicalOr(left, right) => fold_logical!(LogicalOr, left, ||, right),
            kind => panic!("Cannot perform logical fold on type '{:?}'", kind),
        }
    }

    fn fold_unary(self) -> Self {
        todo!()
    }

    fn fold_cast(self) -> Self {
        todo!()
    }

    fn is_const(&self) -> bool {
        matches!(&*self.kind, MlirExprKind::Literal(_))
    }

    fn get_const(self) -> MlirLiteral {
        match *self.kind {
            MlirExprKind::Literal(literal) => literal,
            non_literal => panic!(
                "Cannot unwrap non literal type '{:?}' into literal.",
                non_literal
            ),
        }
    }
}
