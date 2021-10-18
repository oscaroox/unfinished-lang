use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub name: Box<Expression>,
    pub arguments: Vec<Expression>,
}
