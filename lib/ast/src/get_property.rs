use crate::{Expression, Identifier};

#[derive(Debug, Clone, PartialEq)]
pub struct GetProperty {
    pub object: Box<Expression>,
    pub name: Identifier,
}
