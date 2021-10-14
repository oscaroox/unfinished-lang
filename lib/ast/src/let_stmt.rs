use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Let {
    pub id: String,
    pub value: Option<Expression>,
}
