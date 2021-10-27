use crate::{Identifier, Statement};

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Identifier>,
    pub body: Vec<Statement>,
}
