use crate::ast::UnaryOp::Plus;
use crate::ast::*;
use crate::tokens::Literal;
use std::fmt::{Display, Formatter};

// good util for pretty printing the ast
pub fn indent_string(string: String, start_line: usize, indentation: usize) -> String {
    let mut output = String::new();
    for (num, line) in string.lines().enumerate() {
        if (start_line <= num) {
            output.push_str(&format!("{}{}\n", " ".repeat(indentation), line));
        } else {
            output.push_str(line);
            output.push('\n');
        }
    }
    output
}

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
                let value = match literal {
                    Literal::Integer { value, suffix } => value.to_string(),
                    Literal::Float { value, suffix } => value.to_string(),
                    Literal::Char { value } => value.to_string(),
                    Literal::String { value } => value.to_string(),
                };
                format!("{}\n", value)
            }
            Expression::Parenthesized(expr) => {
                let expr = expr.pretty_print(padding.clone(), true, true);
                format!("(\n{}{})\n", expr, padding)
            }
            Expression::Binary(op, left, right) => {
                let right = right.pretty_print(padding.clone(), false, false);
                let left = left.pretty_print(padding.clone(), true, false);
                format!("{}\n{}{}", op, right, left)
            }
            Expression::Unary(op, right) => {
                right.pretty_print(padding.clone(), true, false);
                format!("{}\n", op)
            }
            Expression::PostFix(op, right) => {
                let right = right.pretty_print(padding.clone(), true, false);
                format!("{}{}", right, op)
            }
            Expression::FunctionCall(ident, args) => {
                let mut output = format!("{}{}\n", ident, "(");
                for (i, arg) in args.iter().enumerate() {
                    output += &arg.pretty_print(padding.clone(), i == args.len() - 1, false);
                }
                output += &format!("{})", padding);
                output
            }
            Expression::Member(expr, ident) => {
                let expr = expr.pretty_print(padding.clone(), true, false);
                format!(".\n{}├─ {}\n{}", padding, ident, expr)
            }
            Expression::PointerMember(left, right) => {
                let left = left.pretty_print(padding.clone(), false, false);
                format!("->\n{}{}", left, right)
            }
            Expression::Sizeof(type_or_expr) => {
                format!(
                    "sizeof (\n{})",
                    indent_string(format!("{}", type_or_expr), 0, 4)
                )
            }
            Expression::Index(left, right) => {
                let left = left.pretty_print(padding.clone(), false, false);
                let right = right.pretty_print(padding.clone(), true, false);
                format!("{}[{}]", left, right)
            }
            Expression::Cast(ty, right) => {
                let right = right.pretty_print(padding.clone(), true, false);
                format!("({}) {}", ty, right)
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

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for statement in &self.0 {
            write!(f, "{}", indent_string(format!("{}", statement), 0, 4))?;
        }
        writeln!(f, "}}")
    }
}

impl Display for InitDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::InitDeclaration::*;
        match self {
            Declaration(variable) => writeln!(f, "<init-var-dec> {};", variable),
            Function(function) => write!(f, "<fn> {}", function),
            Struct(structure) => write!(f, "<struct> {}", structure),
        }
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.initializer {
            Some(expr) => write!(
                f,
                "{} = <init-expr> (\n{})",
                self.declaration,
                indent_string(format!("{}", expr), 0, 4)
            ),
            None => write!(f, "{}", self.declaration),
        }
    }
}

impl Display for StructDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} {{", self.ident)?;
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
            if i != self.parameters.len() - 1 || self.varargs {
                write!(f, ", ")?;
            }
        }

        if self.varargs {
            write!(f, "...")?;
        }

        write!(f, ")")?;

        if let Some(body) = &self.body {
            write!(f, " {}", body)?;
        } else {
            write!(f, ";")?;
        }

        Ok(())
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.specifier, self.declarator)?;
        if let Some(ident) = &self.ident {
            write!(f, "{}", ident)?;
        }
        Ok(())
    }
}

impl Display for DeclaratorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::DeclaratorType::*;
        match self {
            Pointer { to } => write!(f, "*{}", to),
            Array { of, size } => match size {
                Some(size) => write!(f, "{}[{}]", of, size),
                any => write!(f, "{}[]", of),
            },
            None => Ok(()),
        }
    }
}

impl Display for TypeQualifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::TypeQualifier::*;
        let str = match self {
            Const => "const",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::TypeSpecifier::*;
        match self {
            Void => write!(f, "void"),
            Char => write!(f, "char"),
            Int => write!(f, "int"),
            Long => write!(f, "long"),
            Double => write!(f, "double"),
            Signed => write!(f, "signed"),
            Unsigned => write!(f, "unsigned"),
            Struct(ident) => write!(f, "struct {}", ident),
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

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::Statement::*;
        write!(f, "<stmt> ");
        match self {
            Expression(expr) => {
                writeln!(
                    f,
                    "<expr> (\n{});",
                    indent_string(format!("{}", expr), 0, 4)
                )
            }
            Declaration(decl) => writeln!(f, "<var-dec> {};", decl),
            Return(expr) => match expr {
                Some(expr) => writeln!(
                    f,
                    "return <expr> (\n{});",
                    indent_string(format!("{}", expr), 0, 4)
                ),
                None => writeln!(f, "return;"),
            },
            Block(block) => writeln!(f, "<block> {}", block),
            _ => panic!("unimplemented statement: {:#?}", self),
        }
    }
}

impl Display for TypeOrExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::TypeOrExpression::*;
        match self {
            Type(ty) => write!(f, "{}", ty),
            Expr(expr) => write!(f, "{}", expr),
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
        use crate::ast::UnaryOp::*;
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
        use crate::ast::AssignOp;
        use crate::ast::BinaryOp::*;
        match self {
            Add => write!(f, "+"),
            Subtract => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Modulo => write!(f, "%"),
            Equal => write!(f, "=="),
            NotEqual => write!(f, "!="),
            GreaterThan => write!(f, ">"),
            GreaterThanEqual => write!(f, ">="),
            LessThan => write!(f, "<"),
            LessThanEqual => write!(f, "<="),
            LogicalAnd => write!(f, "&&"),
            LogicalOr => write!(f, "||"),
            BitwiseAnd => write!(f, "&"),
            BitwiseOr => write!(f, "|"),
            BitwiseXor => write!(f, "^"),
            LeftShift => write!(f, "<<"),
            RightShift => write!(f, ">>"),
            Assign(op) => write!(f, "{}", op),
        }
    }
}

impl Display for AssignOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::AssignOp::*;
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
        use crate::ast::PostfixOp::*;
        let str = match self {
            Increment => "++",
            Decrement => "--",
        };
        write!(f, "{}", str)
    }
}
