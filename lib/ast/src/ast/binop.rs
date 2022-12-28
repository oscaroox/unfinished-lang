
use span_util::Span;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperation {
    Add(Span),
    // TODO while this is convenient, this should be its own expression
    ConcatInterpolation,
    Subtract(Span),
    Multiply(Span),
    Divide(Span),
}

impl std::fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperation::Add(_) => write!(f, "+"),
            BinaryOperation::ConcatInterpolation => write!(f, ""),
            BinaryOperation::Subtract(_) => write!(f, "-"),
            BinaryOperation::Multiply(_) => write!(f, "*"),
            BinaryOperation::Divide(_) => write!(f, "/"),
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
    pub left: Box<Expression>,
    pub op: BinaryOperation,
    pub right: Box<Expression>,
    pub span: Span,
}
