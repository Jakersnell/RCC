use crate::ast::*;
use crate::tokens::Literal;
use std::fmt::{Display, Formatter};

// good util for pretty printing the ast
pub fn indent_string(string: String, indentation: usize) -> String {
    let mut output = String::new();
    for line in string.lines() {
        output.push_str(&format!("{}{}\n", "    ".repeat(indentation), line));
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
                };
                format!("{}\n", value)
            }
            Expression::Binary(op, left, right) => {
                if let BinaryOp::Assign(_) = op {
                    let right = right.pretty_print(padding.clone(), true, false);
                    format!("{}{}\n{}", left, op, right)
                } else {
                    let left = left.pretty_print(padding.clone(), false, false);
                    let right = right.pretty_print(padding.clone(), true, false);
                    format!("{}\n{}{}", op, left, right)
                }
            }
            Expression::Unary(op, right) => {
                right.pretty_print(padding.clone(), true, false);
                format!("{}\n", op)
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
            write!(f, "{}", indent_string(format!("{}", statement), 1))?;
        }
        writeln!(f, "}}")
    }
}

impl Display for InitDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::InitDeclaration::*;
        match self {
            Declaration(variable) => write!(f, "{}", variable),
            Function(function) => write!(f, "{}", function),
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
        let name = match &self.name {
            Some(name) => name.as_ref(),
            None => "<anon function>",
        };
        write!(f, "{} {}", self.ty, name)
    }
}

impl Display for DeclarationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::DeclarationType::*;
        match self {
            Pointer { to } => write!(f, "*{}", to),
            Array { of, size } => match size {
                Some(size) => write!(f, "{}[{}]", of, size),
                None => write!(f, "{}[]", of),
            },
            Type { specifiers, ty } => {
                if let Some(specifiers) = specifiers {
                    for specifier in specifiers {
                        write!(f, "{} ", specifier)?;
                    }
                }
                write!(f, "{}", ty)
            }
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::DataType::*;
        let str = match self {
            Void => "void",
            Char => "char",
            Short => "short",
            Int => "int",
            Long => "long",
            LongLong => "long long",
            Float => "float",
            Double => "double",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for Specifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::Specifier::*;
        let str = match self {
            Const => "const",
            Unsigned => "unsigned",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::Statement::*;
        match self {
            Expression(expr) => write!(f, "{}", expr),
            Declaration(decl) => write!(f, "{}", decl),
            Return(expr) => match expr {
                Some(expr) => write!(f, "return {}", indent_string(format!("{}", expr), 1)),
                None => writeln!(f, "return"),
            },
            Block(block) => write!(f, "{}", block),
        }
    }
}

impl Display for TypeOrIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::ast::TypeOrIdentifier::*;
        match self {
            Type(ty) => write!(f, "{}", ty),
            Identifier(id) => write!(f, "{}", id),
        }
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

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.initializer {
            Some(expr) => write!(
                f,
                "{} = \n{}",
                self.declaration,
                indent_string(format!("{}", expr), 1)
            ),
            None => write!(f, "{}", self.declaration),
        }
    }
}
