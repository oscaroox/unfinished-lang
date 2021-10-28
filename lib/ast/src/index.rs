use crate::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
}
