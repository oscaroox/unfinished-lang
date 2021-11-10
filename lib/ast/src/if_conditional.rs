use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct IfConditional {
    pub condition: Box<Expression>,
    pub then: Vec<Expression>,
    pub not_then: Option<Vec<Expression>>,
}
