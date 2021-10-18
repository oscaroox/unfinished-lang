use crate::Statement;

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
}
