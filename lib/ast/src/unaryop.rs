use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperation {
    Minus,
    Plus,
    Not,
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperation::Minus => write!(f, "-"),
            UnaryOperation::Plus => write!(f, "+"),
            UnaryOperation::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperation,
    pub rhs: Box<Expression>,
}
