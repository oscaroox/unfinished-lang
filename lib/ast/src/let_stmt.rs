use crate::Expression;

#[derive(Debug)]
pub struct Let {
    id: String,
    value: Option<Expression>,
}
