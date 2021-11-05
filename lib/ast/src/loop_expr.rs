use crate::{Expression, Statement};

#[derive(Debug, PartialEq, Clone)]
pub struct LoopExpr {
    pub condition: Box<Expression>,
    pub body: Vec<Statement>,
}
