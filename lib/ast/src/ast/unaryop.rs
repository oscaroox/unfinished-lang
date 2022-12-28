use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperation {
    Minus(Span),
    Plus(Span),
    Not(Span),
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperation::Minus(_) => write!(f, "-"),
            UnaryOperation::Plus(_) => write!(f, "+"),
            UnaryOperation::Not(_) => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperation,
    pub rhs: Box<Expression>,
    pub span: Span,
}
