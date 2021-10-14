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
    pub left: Box<Expression>,
    pub op: BinaryOperation,
    pub right: Box<Expression>,
}
