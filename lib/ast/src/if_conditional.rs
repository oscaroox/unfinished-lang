use crate::{Expression, Statement};

#[derive(Debug, PartialEq, Clone)]
pub struct IfConditional {
    pub condition: Box<Expression>,
    pub then: Vec<Statement>,
    pub not_then: Option<Vec<Statement>>,
}
