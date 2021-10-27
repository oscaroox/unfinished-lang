use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}
