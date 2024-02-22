use crate::tokens::{Literal, Symbol, Token};
use std::fmt::{Display, Formatter};
use std::ptr::write;
use std::sync::Arc;

//

#[derive(Debug)]
pub struct Block(pub Vec<Statement>);

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for statement in &self.0 {
            write!(f, "{}", statement)?;
        }
        write!(f, "}}\n")
    }
}

#[derive(Debug)]
pub enum InitDeclaration {
    Declaration(Declaration, Option<Expression>), // (declaration,  initializer)
    Function(FunctionDeclaration),
}

impl Display for InitDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use InitDeclaration::*;
        match self {
            Declaration(decl, Some(expr)) => write!(f, "{} = {};", decl, expr),
            Declaration(decl, None) => write!(f, "{};", decl),
            Function(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub declaration: Declaration,
    pub identifier: Arc<str>,
    pub parameters: Vec<Declaration>,
    pub varargs: bool, // varargs == true means the last element is of type parameters[parameters.len()-1]
    pub body: Option<Block>,
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}(", self.declaration.ty, self.identifier)?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        if self.varargs {
            write!(f, ", ...")?;
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

#[derive(Debug)]
pub struct Declaration {
    pub name: Arc<str>,
    pub ty: DeclarationType,
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}

#[derive(Debug)]
pub enum DeclarationType {
    Pointer {
        to: Box<DeclarationType>,
    },
    Array {
        of: Box<DeclarationType>,
        size: Option<usize>,
    },
    Function {
        return_type: Box<DeclarationType>,
        parameters: Vec<DeclarationType>,
    },
    Type {
        specifiers: Vec<Specifier>,
        ty: DataType,
    },
}

impl Display for DeclarationType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use DeclarationType::*;
        match self {
            Pointer { to } => write!(f, "*{}", to),
            Array { of, size } => match size {
                Some(size) => write!(f, "{}[{}]", of, size),
                None => write!(f, "{}[]", of),
            },
            Function {
                return_type,
                parameters,
            } => {
                write!(f, "{}(", return_type)?;
                for (i, param) in parameters.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")")
            }
            Type { specifiers, ty } => {
                for specifier in specifiers {
                    write!(f, "{} ", specifier)?;
                }
                write!(f, "{}", ty)
            }
        }
    }
}

#[derive(Debug)]
pub enum DataType {
    Void,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use DataType::*;
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

#[derive(Debug)]
pub enum Specifier {
    Const,
    Unsigned,
}

impl Display for Specifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Specifier::*;
        let str = match self {
            Const => "const",
            Unsigned => "unsigned",
        }
        .to_string();
        write!(f, "{}", str)
    }
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Declaration(Declaration),
    Return(Option<Expression>),
    Block(Block),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Statement::*;
        match self {
            Expression(expr) => write!(f, "{};\n", expr),
            Declaration(decl) => write!(f, "{};\n", decl),
            Return(expr) => match expr {
                Some(expr) => write!(f, "return {};\n", expr),
                None => write!(f, "return;\n"),
            },
            Block(block) => write!(f, "{}", block),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Variable(Arc<str>),
    Sizeof(TypeOrIdentifier),
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    FunctionCall(Arc<str>, Vec<Expression>),
}

impl Expression {
    fn pretty_print(&self, padding: String, last: bool) -> String {
        let output = format!("{}{}", padding, if last { "└──" } else { "├──" });
        let mut padding = padding;
        padding += if last { "   " } else { "│  " };
        let child_output = match self {
            Expression::Variable(ident) => format!(" {}\n", ident),
            Expression::Literal(literal) => {
                let value = match literal {
                    Literal::Integer { value, suffix } => value.to_string(),
                    Literal::Float { value, suffix } => value.to_string(),
                };
                format!(" {}\n", value)
            }
            Expression::Binary(op, left, right) => {
                let left = left.pretty_print(padding.clone(), false);
                let right = right.pretty_print(padding.clone(), true);
                format!(" {}\n{}{}", op, left, right)
            }
            Expression::Unary(op, right) => {
                right.pretty_print(padding.clone(), true);
                format!(" {}\n", op)
            }
            val => unimplemented!("{:#?}", val),
        };

        output + &child_output
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let output = self.pretty_print("".to_string(), true);
        write!(f, "{}", output)
    }
}

#[derive(Debug)]
pub enum TypeOrIdentifier {
    Type(DataType),
    Identifier(Arc<str>),
}

impl Display for TypeOrIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use TypeOrIdentifier::*;
        match self {
            Type(ty) => write!(f, "{}", ty),
            Identifier(id) => write!(f, "{}", id),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Negate,
    LogicalNot,
    BitwiseNot,
}

impl UnaryOp {
    pub fn precedence(&self) -> u8 {
        use UnaryOp::*;
        match self {
            Plus => 4,
            Negate => 4,
            LogicalNot => 4,
            BitwiseNot => 4,
        }
    }
}

impl TryFrom<&Token> for UnaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use UnaryOp::*;
        match value {
            Token::Symbol(Symbol::Plus) => Ok(Plus),
            Token::Symbol(Symbol::Minus) => Ok(Negate),
            Token::Symbol(Symbol::Bang) => Ok(LogicalNot),
            Token::Symbol(Symbol::Tilde) => Ok(BitwiseNot),
            _ => Err(()),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use UnaryOp::*;
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

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    LogicalAnd,
    LogicalOr,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,

    LeftShift,
    RightShift,

    Assign(AssignOp),
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

impl BinaryOp {
    pub fn is_assignment(&self) -> bool {
        matches!(self, BinaryOp::Assign(_))
    }

    pub fn precedence(&self) -> u8 {
        use BinaryOp::*;
        match self {
            Multiply | Divide | Modulo => 3,
            Add | Subtract => 2,
            Assign(_) => 1,
            _ => panic!("Invalid precedence for {:?}", self),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;
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

impl TryFrom<&Token> for BinaryOp {
    type Error = ();

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        use BinaryOp::*;
        match value {
            Token::Symbol(Symbol::Plus) => Ok(Add),
            Token::Symbol(Symbol::Minus) => Ok(Subtract),
            Token::Symbol(Symbol::Star) => Ok(Multiply),
            Token::Symbol(Symbol::Slash) => Ok(Divide),
            Token::Symbol(Symbol::Modulo) => Ok(Modulo),

            Token::Symbol(Symbol::EqualEqual) => Ok(Equal),
            Token::Symbol(Symbol::BangEqual) => Ok(NotEqual),
            Token::Symbol(Symbol::GreaterThan) => Ok(GreaterThan),
            Token::Symbol(Symbol::GreaterThanEqual) => Ok(GreaterThanEqual),
            Token::Symbol(Symbol::LessThan) => Ok(LessThan),
            Token::Symbol(Symbol::LessThanEqual) => Ok(LessThanEqual),

            Token::Symbol(Symbol::Ampersand) => Ok(BitwiseAnd),
            Token::Symbol(Symbol::Pipe) => Ok(BitwiseOr),
            Token::Symbol(Symbol::Caret) => Ok(BitwiseXor),
            Token::Symbol(Symbol::LeftShift) => Ok(LeftShift),
            Token::Symbol(Symbol::RightShift) => Ok(RightShift),

            Token::Symbol(Symbol::Equal) => Ok(Assign(AssignOp::Assign)),
            Token::Symbol(Symbol::PlusEqual) => Ok(Assign(AssignOp::Plus)),
            Token::Symbol(Symbol::MinusEqual) => Ok(Assign(AssignOp::Minus)),
            Token::Symbol(Symbol::StarEqual) => Ok(Assign(AssignOp::Multiply)),
            Token::Symbol(Symbol::SlashEqual) => Ok(Assign(AssignOp::Divide)),
            Token::Symbol(Symbol::ModuloEqual) => Ok(Assign(AssignOp::Modulo)),
            Token::Symbol(Symbol::AmpersandEqual) => Ok(Assign(AssignOp::BitwiseAnd)),
            Token::Symbol(Symbol::PipeEqual) => Ok(Assign(AssignOp::BitwiseOr)),
            Token::Symbol(Symbol::CaretEqual) => Ok(Assign(AssignOp::BitwiseXor)),
            Token::Symbol(Symbol::LeftShiftEqual) => Ok(Assign(AssignOp::LeftShift)),
            Token::Symbol(Symbol::RightShiftEqual) => Ok(Assign(AssignOp::RightShift)),

            _ => Err(()),
        }
    }
}
