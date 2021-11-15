use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Identifier>,
    // the parser makes sure this is a block expression
    pub body: Box<Expression>,
    pub is_static: bool,
}
