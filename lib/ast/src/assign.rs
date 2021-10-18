use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Identifier,
    pub rhs: Box<Expression>,
}
