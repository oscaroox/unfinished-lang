use crate::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct SetIndex {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
    pub value: Box<Expression>,
}
