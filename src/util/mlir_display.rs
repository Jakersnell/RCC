use std::fmt::{Display, Formatter};

use crate::data::mlir::*;
use crate::util::display_utils::indent_string;

impl Display for MlirStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MlirStmt::Block(block) => write!(f, "{{\n{}\n}}", block),
            MlirStmt::Expression(expr) => write!(f, "{};", expr),
            MlirStmt::VariableDeclaration(var) => write!(f, "{};", var),
            MlirStmt::Label(ident) => write!(f, "label {};", ident),
            MlirStmt::Goto(ident) => write!(f, "goto {};", ident),
            MlirStmt::CondGoto(condition, then, _else) => {
                write!(
                    f,
                    "cond-goto | cond: {}; then: {}; else:{};",
                    condition, then, _else
                )
            }
            MlirStmt::Return(value) => {
                write!(f, "return")?;
                if let Some(value) = value.as_ref() {
                    write!(f, " {}", value)?;
                }
                write!(f, ";")
            }
        }
    }
}

impl Display for MlirBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let last = self.0.iter().last();
        for stmt in &self.0 {
            write!(f, "{}", stmt)?;
            if stmt != last.unwrap() {
                writeln!(f)?
            }
        }
        Ok(())
    }
}

impl Display for MlirFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}(", self.ty, self.ident)?;

        for param in &self.parameters {
            write!(f, " {},", &param.value)?;
        }
        writeln!(f, ") {{")?;
        writeln!(f, "{}", self.body);
        write!(f, "}}")
    }
}

impl Display for MlirVarInit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MlirVarInit::Expr(expression) => write!(f, "{}", expression),
            MlirVarInit::Array(elements) => {
                write!(f, "{{ ");
                for element in elements {
                    write!(f, "{}", element);
                }
                write!(f, " }}")
            }
        }
    }
}

impl Display for CastType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string_repr = match self {
            CastType::ArrayToPointer => "array-to-pointer",
            CastType::PointerToPointer => "pointer-to-pointer",
            CastType::PointerToLong => "pointer-to-long",
            CastType::LongToPointer => "long-to-pointer",
            CastType::SignedToUnsigned => "signed-to-unsigned",
            CastType::UnsignedToSigned => "unsigned-to-signed",
            CastType::CharToInt => "char-to-int",
            CastType::IntToFloat => "int-to-float",
            CastType::IntToLong => "int-to-long",
            CastType::FloatToDouble => "float-to-double",
            CastType::LongToDouble => "long-to-double",
            CastType::DoubleToLong => "double-to-long",
            CastType::LongToInt => "long-to-int",
            CastType::IntToChar => "int-to-char",
            CastType::DoubleToFloat => "double-to-float",
            CastType::FloatToInt => "float-to-int",
        };
        write!(f, "{}", string_repr)
    }
}

impl Display for MlirLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MlirLiteral::Char(val) => write!(f, "{}", val),
            MlirLiteral::UChar(val) => write!(f, "{}", val),
            MlirLiteral::Int(val) => write!(f, "{}", val),
            MlirLiteral::UInt(val) => write!(f, "{}", val),
            MlirLiteral::Long(val) => write!(f, "{}", val),
            MlirLiteral::ULong(val) => write!(f, "{}", val),
            MlirLiteral::Float(val) => write!(f, "{}", val),
            MlirLiteral::Double(val) => write!(f, "{}", val),
            MlirLiteral::String(string) => {
                write!(
                    f,
                    "\"{}\"",
                    String::from_utf8(string.clone()).expect("String should be valid utf-8")
                )
            }
        }
    }
}

impl Display for MlirExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &*self.kind {
            MlirExprKind::Literal(literal) => write!(f, "{}", literal),
            MlirExprKind::Variable(ident) => write!(f, "{}", ident),
            MlirExprKind::PostIncrement(expression) => write!(f, "{}++", expression),
            MlirExprKind::PostDecrement(expression) => write!(f, "++{}", expression),
            MlirExprKind::Negate(expression) => write!(f, "-{}", expression),
            MlirExprKind::LogicalNot(expression) => write!(f, "!{}", expression),
            MlirExprKind::BitwiseNot(expression) => write!(f, "~{}", expression),
            MlirExprKind::Deref(expression) => write!(f, "*{}", expression),
            MlirExprKind::AddressOf(expression) => write!(f, "&{}", expression),
            MlirExprKind::Add(left, right) => write!(f, "({} + {})", left, right),
            MlirExprKind::Sub(left, right) => write!(f, "({} - {})", left, right),
            MlirExprKind::Mul(left, right) => write!(f, "({} + {})", left, right),
            MlirExprKind::Div(left, right) => write!(f, "({} / {})", left, right),
            MlirExprKind::Mod(left, right) => write!(f, "({} % {})", left, right),
            MlirExprKind::Equal(left, right) => write!(f, "({} == {})", left, right),
            MlirExprKind::NotEqual(left, right) => write!(f, "({} != {})", left, right),
            MlirExprKind::GreaterThan(left, right) => write!(f, "({} < {})", left, right),
            MlirExprKind::GreaterThanEqual(left, right) => write!(f, "({} <= {})", left, right),
            MlirExprKind::LessThan(left, right) => write!(f, "({} > {})", left, right),
            MlirExprKind::LessThanEqual(left, right) => write!(f, "({} >= {})", left, right),
            MlirExprKind::LogicalAnd(left, right) => write!(f, "({} && {})", left, right),
            MlirExprKind::LogicalOr(left, right) => write!(f, "({} || {})", left, right),
            MlirExprKind::BitwiseAnd(left, right) => write!(f, "({} && {})", left, right),
            MlirExprKind::BitwiseOr(left, right) => write!(f, "({} | {})", left, right),
            MlirExprKind::BitwiseXor(left, right) => write!(f, "({} ^ {})", left, right),
            MlirExprKind::LeftShift(left, right) => write!(f, "({} << {})", left, right),
            MlirExprKind::RightShift(left, right) => write!(f, "({} >> {})", left, right),
            MlirExprKind::Assign(left, right) => write!(f, "({} = {})", left, right),
            MlirExprKind::FunctionCall {
                location,
                ident,
                args,
            } => {
                if let Some(loc) = location {
                    write!(f, "<external-'{}'-fn-call: {}> ", loc, ident)?;
                } else {
                    write!(f, "<internal-fn-call: {}>", ident)?;
                }
                write!(f, "")
            }
            MlirExprKind::Index(left, right) => write!(f, "(({})[{}])", left, right),
            MlirExprKind::Member(left, right) => write!(f, "{}.{}", left, right),
            MlirExprKind::Cast(cast_type, expression) => {
                write!(f, "(<cast-{}>({}))", cast_type, expression)
            }
        }
    }
}

impl Display for MlirVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }

        write!(f, "{} {}", self.ty.value, self.ident)?;

        if let Some(init) = self.initializer.as_ref() {
            write!(f, " = {}", &init.value)?;
        }

        Ok(())
    }
}

impl Display for MlirStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} <{} bytes> {{", self.ident.value, self.size)?;
        for field in &self.members {
            writeln!(f, "{};", field.value)?;
        }
        write!(f, "}}")
    }
}

impl Display for MlirModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        macro_rules! indent_write {
            ($item:expr) => {
                writeln!(f, "{}", indent_string(format!("{}", $item), 0, 1))?;
            };
        }

        write!(f, "{{\n Structs {{")?;
        for (_, _struct) in self.structs.iter() {
            indent_write!(_struct);
        }

        write!(f, "}} Global Variables {{ ")?;
        for (_, var) in self.globals.iter() {
            indent_write!(var);
        }

        write!(f, "}}\n Functions {{")?;
        for (_, func) in self.functions.iter() {
            indent_write!(func);
        }

        write!(f, "}}")
    }
}

impl Display for MlirTypeKind {
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
        use crate::data::mlir::MlirTypeKind::*;
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
