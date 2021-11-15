use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct IfConditional {
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub not_then: Option<Box<Expression>>,
}
