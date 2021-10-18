use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperation {
    Minus,
    Plus,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperation,
    pub rhs: Box<Expression>,
}
