use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ImplicitReturn {
    pub value: Box<Expression>,
}
