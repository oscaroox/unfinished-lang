use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Identifier>,
    pub body: Vec<Expression>,
    pub is_static: bool,
}
