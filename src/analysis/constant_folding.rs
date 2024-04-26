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

macro_rules! unary_fold_literal {
    ($mac:ident, $op:tt, $unit:expr, $($lit_kind:ident),+) => {
        match $unit.get_const() {
            $ (
                MlirLiteral::$lit_kind(unit) => $mac!(unit, $lit_kind)
            )+
            unit => panic!("{}{:?} is not a supported operation.", unit, stringify!($op))
        }
    }
}

macro_rules! unary_fold {
    ($span:expr, $ty:expr, $kind:ident, $op:tt, $unit:expr, $folding_mac:ident, $mac:ident, $($lit_kind:ident),+) => {{
        let unit = $unit.fold();
        let expr_kind = if unit.is_const() {
            MlirExprKind::Literal($folding_mac!($op, $unit, $($lit_kind)+))
        } else {
            MlirExprKind::$kind(unit)
        };
        MlirExpr {
            kind: Box::new(expr_kind),
            is_lval: false,
            span: $span,
            ty: $ty
        }
    }};
}

macro_rules! debug_assert_numeric_type_validity {
    ($lit:expr) => {
        // Out of debug mode this will be compiled out, and not exist in production code
        if cfg!(debug_assertions) {
            if let MlirExprKind::Literal(literal) = &*$lit.kind {
                macro_rules! assert_type {
                    ($ty_kind:expr) => {
                        if $lit.ty.kind != $ty_kind {
                            panic!("Invalid literal and type pairing. {:#?}", $lit);
                        }
                    };
                }
                match literal {
                    MlirLiteral::Char(_) => assert_type!(MlirTypeKind::Char(false)),
                    MlirLiteral::UChar(_) => assert_type!(MlirTypeKind::Char(true)),
                    MlirLiteral::Int(_) => assert_type!(MlirTypeKind::Int(false)),
                    MlirLiteral::UInt(_) => assert_type!(MlirTypeKind::Int(true)),
                    MlirLiteral::Long(_) => assert_type!(MlirTypeKind::Long(false)),
                    MlirLiteral::ULong(_) => assert_type!(MlirTypeKind::Long(true)),
                    MlirLiteral::Float(_) => assert_type!(MlirTypeKind::Float),
                    MlirLiteral::Double(_) => assert_type!(MlirTypeKind::Double),
                    literal => panic!("Literal type '{:?}' is non numeric! {:#?}", literal, $lit),
                }
            } else {
                panic!("Type is not a literal!")
            }
        }
    };
}

impl MlirExpr {
    pub(in crate::analysis) fn fold(self) -> Self {
        if self.is_const() {
            debug_assert_numeric_type_validity!(self);
        }
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

            MlirExprKind::LogicalAnd(_, _) | MlirExprKind::LogicalOr(_, _) => self.logical_fold(),

            MlirExprKind::Negate(_) => self.fold_negate(),
            MlirExprKind::LogicalNot(_) => self.fold_logical_not(),
            MlirExprKind::BitwiseNot(_) => self.fold_bitwise_not(),

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
                    bin_fold_literal,
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
                    bin_fold_literal,
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
                    bin_fold_literal,
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

    fn fold_negate(self) -> Self {
        let MlirExpr { span, ty, kind, .. } = self;
        let unit = match *kind {
            MlirExprKind::Negate(unit) => unit.fold(),
            kind => panic!(
                "Expression kind '{:?}' is not 'MlirExprKind::Negate' and cannot be folded as such.",
                kind
            ),
        };
        let expr_kind = if unit.is_const() {
            let literal = match unit.get_const() {
                MlirLiteral::Char(x) => MlirLiteral::Char(-x),
                MlirLiteral::UChar(x) => MlirLiteral::Char(-(x as i8)),
                MlirLiteral::Int(x) => MlirLiteral::Int(-x),
                MlirLiteral::UInt(x) => MlirLiteral::Int(-(x as i32)),
                MlirLiteral::Long(x) => MlirLiteral::Long(-x),
                MlirLiteral::ULong(x) => MlirLiteral::Long(-(x as i64)),
                MlirLiteral::Float(x) => MlirLiteral::Float(-x),
                MlirLiteral::Double(x) => MlirLiteral::Double(-x),
                literal => panic!("Literal type '{:?}' is not able to be negated!", literal),
            };
            MlirExprKind::Literal(literal)
        } else {
            MlirExprKind::Negate(unit)
        };
        MlirExpr {
            kind: Box::new(expr_kind),
            is_lval: false,
            span,
            ty,
        }
    }

    fn fold_logical_not(self) -> Self {
        let MlirExpr { span, ty, kind, .. } = self;
        let unit = match *kind {
            MlirExprKind::LogicalNot(unit) => unit.fold(),
            kind => panic!(
                "Expression kind '{:?}' is not 'MlirExprKind::LogicalNot' and cannot be folded as such.",
                kind
            ),
        };
        let expr_kind = if unit.is_const() {
            macro_rules! not {
                ($x:expr) => {
                    MlirLiteral::Int(if $x != 0 { 1 } else { 0 })
                };
            }
            macro_rules! float_not {
                ($x:expr) => {
                    MlirLiteral::Int(if $x != 0.0 { 1 } else { 0 })
                };
            }
            MlirExprKind::Literal(match unit.get_const() {
                MlirLiteral::Char(x) => not!(x),
                MlirLiteral::UChar(x) => not!(x),
                MlirLiteral::Int(x) => not!(x),
                MlirLiteral::UInt(x) => not!(x),
                MlirLiteral::Long(x) => not!(x),
                MlirLiteral::ULong(x) => not!(x),
                MlirLiteral::Float(x) => float_not!(x),
                MlirLiteral::Double(x) => float_not!(x),
                literal => panic!("Cannot perform logical not on type '{:?}'!", literal),
            })
        } else {
            MlirExprKind::LogicalNot(unit)
        };
        MlirExpr {
            kind: Box::new(expr_kind),
            is_lval: false,
            span,
            ty,
        }
    }

    fn fold_bitwise_not(self) -> Self {
        let MlirExpr { span, ty, kind, .. } = self;
        let unit = match *kind {
            MlirExprKind::LogicalNot(unit) => unit.fold(),
            kind => panic!(
                "Expression kind '{:?}' is not 'MlirExprKind::LogicalNot' and cannot be folded as such.",
                kind
            ),
        };
        let expr_kind = if unit.is_const() {
            MlirExprKind::Literal(match unit.get_const() {
                MlirLiteral::Char(x) => MlirLiteral::Char(!x),
                MlirLiteral::UChar(x) => MlirLiteral::UChar(!x),
                MlirLiteral::Int(x) => MlirLiteral::Int(!x),
                MlirLiteral::UInt(x) => MlirLiteral::UInt(!x),
                MlirLiteral::Long(x) => MlirLiteral::Long(!x),
                MlirLiteral::ULong(x) => MlirLiteral::ULong(!x),
                literal => panic!("Cannot perform bitwise not on type '{:?}'!", literal),
            })
        } else {
            MlirExprKind::BitwiseNot(unit)
        };
        MlirExpr {
            kind: Box::new(expr_kind),
            is_lval: false,
            span,
            ty,
        }
    }

    fn fold_cast(self) -> Self {
        let MlirExpr {
            span,
            ty,
            kind,
            is_lval,
        } = self;
        let (cast_type, unit) = match *kind {
            MlirExprKind::Cast(cast_type, casted) => (cast_type, casted.fold()),
            kind => panic!(
                "Expression kind '{:?}' is not 'MlirExprKind::LogicalNot' and cannot be folded as such.",
                kind
            ),
        };
        let kind = if unit.is_const() && cast_type.can_fold() {
            debug_assert_numeric_type_validity!(unit);
            let _const = unit.get_const();
            let literal = match cast_type {
                CastType::SignedToUnsigned => {
                    match _const {
                        MlirLiteral::Char(x) => MlirLiteral::UChar(x as u8),
                        MlirLiteral::Int(x) => MlirLiteral::UInt(x as u32),
                        MlirLiteral::Long(x) => MlirLiteral::ULong(x as u64),
                        literal => {
                            panic!("Cannot perform signed to unsigned conversion on literal type '{:?}'", literal)
                        }
                    }
                }
                CastType::UnsignedToSigned => {
                    match _const {
                        MlirLiteral::UChar(x) => MlirLiteral::Char(x as i8),
                        MlirLiteral::UInt(x) => MlirLiteral::Int(x as i32),
                        MlirLiteral::ULong(x) => MlirLiteral::Long(x as i64),
                        literal => {
                            panic!("Cannot perform unsigned to signed conversion on literal type '{:?}'", literal)
                        }
                    }
                }
                CastType::CharToInt => match _const {
                    MlirLiteral::Char(x) => MlirLiteral::Int(x as i32),
                    MlirLiteral::UChar(x) => MlirLiteral::UInt(x as u32),
                    literal => {
                        panic!(
                            "Cannot perform char to int conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::IntToFloat => match _const {
                    MlirLiteral::Int(x) => MlirLiteral::Float(x as f32),
                    MlirLiteral::UInt(x) => MlirLiteral::Float(x as f32),
                    literal => {
                        panic!(
                            "Cannot perform char to int conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::IntToLong => match _const {
                    MlirLiteral::Int(x) => MlirLiteral::Long(x as i64),
                    MlirLiteral::UInt(x) => MlirLiteral::ULong(x as u64),
                    literal => {
                        panic!(
                            "Cannot perform int to long conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::FloatToDouble => match _const {
                    MlirLiteral::Float(x) => MlirLiteral::Double(x as f64),
                    literal => {
                        panic!(
                            "Cannot perform float to double conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::LongToDouble => match _const {
                    MlirLiteral::Long(x) => MlirLiteral::Double(x as f64),
                    MlirLiteral::ULong(x) => MlirLiteral::Double(x as f64),
                    literal => {
                        panic!(
                            "Cannot perform long to double conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::LongToDouble => match _const {
                    MlirLiteral::Double(x) => MlirLiteral::Long(x as i64),
                    literal => {
                        panic!(
                            "Cannot perform double to long conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::LongToInt => match _const {
                    MlirLiteral::Long(x) => MlirLiteral::Int(x as i32),
                    MlirLiteral::ULong(x) => MlirLiteral::UInt(x as u32),
                    literal => {
                        panic!(
                            "Cannot perform long to int conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::IntToChar => match _const {
                    MlirLiteral::Int(x) => MlirLiteral::Char(x as i8),
                    MlirLiteral::UInt(x) => MlirLiteral::UChar(x as u8),
                    literal => {
                        panic!(
                            "Cannot perform int to char conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::DoubleToFloat => match _const {
                    MlirLiteral::Double(x) => MlirLiteral::Float(x as f32),
                    literal => {
                        panic!(
                            "Cannot perform double to float conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                CastType::FloatToInt => match _const {
                    MlirLiteral::Float(x) => MlirLiteral::Int(x as i32),
                    literal => {
                        panic!(
                            "Cannot perform float to int conversion on literal type '{:?}'",
                            literal
                        )
                    }
                },
                _ => unreachable!(),
            };
            MlirExprKind::Literal(literal)
        } else {
            MlirExprKind::Cast(cast_type, unit)
        };
        MlirExpr {
            kind: Box::new(kind),
            is_lval,
            span,
            ty,
        }
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

impl CastType {
    fn can_fold(&self) -> bool {
        matches!(
            self,
            CastType::SignedToUnsigned
                | CastType::UnsignedToSigned
                | CastType::CharToInt
                | CastType::IntToFloat
                | CastType::IntToLong
                | CastType::FloatToDouble
                | CastType::LongToDouble
                | CastType::DoubleToLong
                | CastType::LongToInt
                | CastType::IntToChar
                | CastType::DoubleToFloat
                | CastType::FloatToInt
        )
    }
}
