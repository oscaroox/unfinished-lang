use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperation {
    Add,
    Substract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
    left: Box<Expression>,
    op: BinaryOperation,
    right: Box<Expression>,
}
