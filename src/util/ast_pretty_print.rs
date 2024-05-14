use std::fmt::{Display, Formatter};

use crate::data::ast::*;
use crate::data::tokens::Literal;
use crate::util::display_utils;

impl Expression {
    pub(crate) fn pretty_print(&self, padding: String, last: bool, is_root: bool) -> String {
        let mut padding = padding;
        let mut output;
        if is_root {
            output = padding.clone();
        } else {
            output = format!("{}{}", padding, if last { "└─ " } else { "├─ " });
            padding += if last { "   " } else { "│  " };
        };
        let child_output = match self {
            Expression::Variable(ident) => format!("{}\n", ident),
            Expression::Literal(literal) => {
                let value = match &literal.value {
                    Literal::Integer { value, suffix } => value.to_string(),
                    Literal::Float { value, suffix } => value.to_string(),
                    Literal::Char { value } => format!("'{}'", value),
                    Literal::String { value } => format!("\"{}\"", value),
                };
                format!("{}\n", value)
            }
            Expression::Parenthesized(expr) => {
                let expr = expr.pretty_print(padding.clone(), true, true);
                let expr = display_utils::indent_string(expr, 0, 4);
                format!("(\n{}{})\n", expr, padding)
            }
            Expression::Binary(op, left, right) => {
                let right = right.pretty_print(padding.clone(), false, false);
                let left = left.pretty_print(padding.clone(), true, false);
                format!("{}\n{}{}", op, right, left)
            }
            Expression::Unary(op, right) => {
                let right = right.pretty_print(padding.clone(), true, false);
                format!("{}\n{}", op, right)
            }
            Expression::PostFix(op, right) => {
                let right = right.pretty_print(padding.clone(), true, false);
                format!("{} <post> \n{}", op, right)
            }
            Expression::FunctionCall(ident, args) => {
                let mut output = format!("<fn-call> {}{}\n", **ident, "(");
                for (i, arg) in args.iter().enumerate() {
                    let arg = &arg.pretty_print(padding.clone(), i == args.len() - 1, false);
                    let arg = display_utils::indent_string(arg.to_string(), 0, 4);
                    output += &arg;
                }
                output += &format!("{})", padding);
                output
            }
            Expression::Member(expr, ident) => {
                let expr = expr.pretty_print(padding.clone(), true, false);
                format!(".\n{}├─ {}\n{}", padding, **ident, expr)
            }
            Expression::PointerMember(expr, ident) => {
                let expr = expr.pretty_print(padding.clone(), true, false);
                format!("->\n{}├─ {}\n{}", padding, **ident, expr)
            }
            Expression::Sizeof(type_or_expr) => {
                format!(
                    "sizeof (\n{})",
                    display_utils::indent_string(format!("{}", type_or_expr), 0, 4)
                )
            }
            Expression::Index(left, right) => {
                let left = left.pretty_print(padding.clone(), false, false);
                let right = right.pretty_print(padding.clone(), true, false);
                format!("{}[{}]", left, right)
            }
            Expression::Cast(ty, right) => {
                let right = right.pretty_print(padding.clone(), true, true);
                let right = display_utils::indent_string(right, 0, 4);
                format!("<cast> {} (\n{}\n)", ty, right)
            }
            Expression::ArrayInitializer(initializer) => {
                let mut output = "{\n".to_string();
                for (i, expr) in initializer.iter().enumerate() {
                    output +=
                        &expr.pretty_print(padding.clone(), i == initializer.len() - 1, false);
                }
                output += &"}";
                output
            }
        };
        output + &child_output
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output = self.pretty_print("".to_string(), true, true);
        write!(f, "{}", output)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::data::ast::Statement::*;
        use crate::util::display_utils;
        write!(f, "<stmt> ");
        match self {
            Expression(expr) => {
                write!(
                    f,
                    "<expr> (\n{});",
                    display_utils::indent_string(format!("{}", expr), 0, 4)
                )
            }
            Declaration(decl) => write!(f, "<var-dec> {};", decl),
            Return(expr) => match expr {
                Some(expr) => write!(
                    f,
                    "return <expr> (\n{});",
                    display_utils::indent_string(format!("{}", expr), 0, 4)
                ),
                None => write!(f, "return;"),
            },
            If(cond, then, els) => {
                write!(
                    f,
                    "if <cond> (\n{}\n) <then> {{\n{}\n}}",
                    display_utils::indent_string(format!("{}", cond), 0, 4),
                    display_utils::indent_string(format!("{}", then), 0, 4),
                )?;
                if let Some(els) = els {
                    write!(f, " else {}", els)?;
                }
                Ok(())
            }
            While(cond, body) => write!(
                f,
                "while <cond> (\n{}) <body> {{\n{}}}",
                display_utils::indent_string(format!("{}", cond), 0, 4),
                display_utils::indent_string(format!("{}", body), 0, 4)
            ),
            For(init, cond, post, body) => write!(
                f,
                "for <init> (\n{}), <cond> (\n{}), <post> (\n{}) <body> {{\n{}\n}}",
                if let Some(init) = init {
                    display_utils::indent_string(format!("{}", init), 0, 4)
                } else {
                    "None".to_string()
                },
                if let Some(cond) = cond {
                    display_utils::indent_string(format!("{}", cond), 0, 4)
                } else {
                    "None".to_string()
                },
                if let Some(post) = post {
                    display_utils::indent_string(format!("{}", post), 0, 4)
                } else {
                    "None".to_string()
                },
                display_utils::indent_string(format!("{}", body), 0, 4)
            ),
            Block(block) => write!(f, "<block> {}", block),
            Break => write!(f, "break;"),
            Continue => write!(f, "continue;"),
            Empty => write!(f, "<empty statement>;"),
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for statement in &self.0 {
            write!(
                f,
                "{}",
                display_utils::indent_string(format!("{}", statement), 0, 4)
            )?;
        }
        writeln!(f, "}}")
    }
}

impl Display for InitDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::data::ast::InitDeclaration::*;
        match self {
            Declaration(variable) => writeln!(f, "<init-var-dec> {};", variable),
            Function(function) => write!(f, "<fn> {}", function),
            Struct(structure) => write!(f, "<struct> {}", structure),
        }
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", *self.declaration);
        if self.is_array {
            write!(f, "[")?;
            if let Some(array_size) = self.array_size {
                write!(f, "{}", array_size)?;
            }
            write!(f, "]")?;
        }
        if let Some(expr) = &self.initializer {
            write!(
                f,
                " = <init-expr> (\n{})",
                display_utils::indent_string(format!("{}", expr), 0, 4)
            );
        }
        Ok(())
    }
}

impl Display for StructDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.declaration)?;
        for member in &self.members {
            writeln!(f, "    {};", member)?;
        }
        writeln!(f, "}};")
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.declaration)?;
        for (i, param) in self.parameters.iter().enumerate() {
            write!(f, "{}", param)?;
            if i != self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        write!(f, " {}", self.body)
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.specifier)?;
        if let Some(ident) = &self.ident {
            write!(f, " {}", ident)?;
        }
        Ok(())
    }
}

impl Display for TypeQualifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::data::ast::TypeQualifier::*;
        let str = match self {
            Const => "const",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSpecifier::Void => write!(f, "void"),
            TypeSpecifier::Char => write!(f, "char"),
            TypeSpecifier::Int => write!(f, "int"),
            TypeSpecifier::Long => write!(f, "long"),
            TypeSpecifier::Double => write!(f, "double"),
            TypeSpecifier::Signed => write!(f, "signed"),
            TypeSpecifier::Unsigned => write!(f, "unsigned"),
            TypeSpecifier::Struct(ident) => write!(f, "struct {}", ident),
        }
    }
}

impl Display for StorageSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            StorageSpecifier::Static => "static",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for TypeOrExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeOrExpression::Type(ty) => write!(f, "{}", ty),
            TypeOrExpression::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for DeclarationSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_field {
            ($field:expr) => {
                for (i, item) in $field.iter().enumerate() {
                    write!(f, "{}", item)?;
                    if i != $field.len() - 1 {
                        write!(f, " ")?;
                    }
                }
            };
        }
        write_field!(self.specifiers);
        write_field!(self.qualifiers);
        write_field!(self.ty);
        Ok(())
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::data::ast::UnaryOp::*;
        let str = match self {
            Plus => "+",
            Negate => "-",
            LogicalNot => "!",
            BitwiseNot => "~",
            Increment => "++",
            Decrement => "--",
            Deref => "*",
            AddressOf => "&",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::GreaterThanEqual => write!(f, ">="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::LessThanEqual => write!(f, "<="),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
            BinaryOp::BitwiseAnd => write!(f, "&"),
            BinaryOp::BitwiseOr => write!(f, "|"),
            BinaryOp::BitwiseXor => write!(f, "^"),
            BinaryOp::LeftShift => write!(f, "<<"),
            BinaryOp::RightShift => write!(f, ">>"),
            BinaryOp::Assign(op) => write!(f, "{}", op),
        }
    }
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::data::ast::AssignOp::*;
        let str = match self {
            Assign => "=",
            Plus => "+",
            Minus => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            BitwiseAnd => "&",
            BitwiseOr => "|",
            BitwiseXor => "^",
            LeftShift => "<<",
            RightShift => ">>",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for PostfixOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::data::ast::PostfixOp::*;
        let str = match self {
            Increment => "++",
            Decrement => "--",
        };
        write!(f, "{}", str)
    }
}
