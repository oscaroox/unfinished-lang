use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnExpr {
    pub value: Box<Option<Expression>>,
}
