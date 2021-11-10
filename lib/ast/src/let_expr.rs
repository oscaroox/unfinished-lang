use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct LetExpr {
    pub name: Identifier,
    pub value: Option<Box<Expression>>,
}
