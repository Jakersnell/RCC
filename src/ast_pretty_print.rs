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
            output = "".to_string();
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
            Expression::Binary(op, left, right) => {
                let right = right.pretty_print(padding.clone(), false, false);
                let left = left.pretty_print(padding.clone(), true, false);
                format!("{}\n{}{}", op, right, left)
            }
            Expression::Unary(op, right) => {
                right.pretty_print(padding.clone(), true, false);
                format!("{}\n", op)
            }
            Expression::Assignment(op, left, right) => {
                let right = right.pretty_print(padding.clone(), true, false);
                format!("{} {}\n{}", left, op, right)
            }
            val => unimplemented!("{:#?}", val),
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
            Declaration(variable) => write!(f, "<init-var-dec> {}", variable),
            Function(function) => write!(f, "<fn> {}", function),
        }
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
        write!(f, "{} {}", self.specifier, self.declarator)
    }
}

impl Display for Declarator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::Declarator::*;
        match self {
            Pointer { to } => write!(f, "*{}", to),
            Array { of, size } => match size {
                Some(size) => write!(f, "{}[{}]", of, size),
                any => write!(f, "{}[]", of),
            },
            Unit { ident } => write!(f, "{}", ident),
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
        let str = match self {
            Void => "void",
            Char => "char",
            Int => "int",
            Long => "long",
            Double => "double",
            Signed => "signed",
            Unsigned => "unsigned",
        }
        .to_string();
        write!(f, "{}", str)
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
            Expression(expr) => write!(f, " <expr> {}", expr),
            Declaration(decl) => write!(f, " <var-dec> {}", decl),
            Return(expr) => match expr {
                Some(expr) => write!(
                    f,
                    "return <expr> (\n{})",
                    indent_string(format!("{}", expr), 0, 4)
                ),
                None => writeln!(f, "return;"),
            },
            Block(block) => write!(f, "<block> {}", block),
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
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::AssignOp;
        use crate::ast::BinaryOp::*;
        let str = match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            Equal => "==",
            NotEqual => "!=",
            GreaterThan => ">",
            GreaterThanEqual => ">=",
            LessThan => "<",
            LessThanEqual => "<=",
            LogicalAnd => "&&",
            LogicalOr => "||",
            BitwiseAnd => "&",
            BitwiseOr => "|",
            BitwiseXor => "^",
            LeftShift => "<<",
            RightShift => ">>",
            Assign(AssignOp::Assign) => "=",
            Assign(AssignOp::Plus) => "+=",
            Assign(AssignOp::Minus) => "-=",
            Assign(AssignOp::Multiply) => "*=",
            Assign(AssignOp::Divide) => "/=",
            Assign(AssignOp::Modulo) => "%=",
            Assign(AssignOp::BitwiseAnd) => "&=",
            Assign(AssignOp::BitwiseOr) => "|=",
            Assign(AssignOp::BitwiseXor) => "^=",
            Assign(AssignOp::LeftShift) => "<<=",
            Assign(AssignOp::RightShift) => ">>=",
        }
        .to_string();
        write!(f, "{}", str)
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

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.initializer {
            Some(expr) => writeln!(
                f,
                "{} = <init-expr> (\n{})",
                self.declaration,
                indent_string(format!("{}", expr), 0, 4)
            ),
            None => writeln!(f, "{};", self.declaration),
        }
    }
}
