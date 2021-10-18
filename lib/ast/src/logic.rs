use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum LogicOperation {
    Or,
    And,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logic {
    pub lhs: Box<Expression>,
    pub op: LogicOperation,
    pub rhs: Box<Expression>,
}
