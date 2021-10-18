use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub name: Box<Expression>,
    pub arguments: Vec<Expression>,
}
