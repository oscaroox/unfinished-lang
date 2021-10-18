use crate::{Identifier, Statement};

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub params: Vec<Identifier>,
    pub body: Vec<Statement>,
}
