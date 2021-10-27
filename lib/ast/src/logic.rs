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

impl std::fmt::Display for LogicOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicOperation::Or => write!(f, "||"),
            LogicOperation::And => write!(f, "&&"),
            LogicOperation::Equal => write!(f, "=="),
            LogicOperation::NotEqual => write!(f, "!="),
            LogicOperation::LessThan => write!(f, "<"),
            LogicOperation::LessThanEqual => write!(f, "<="),
            LogicOperation::GreaterThan => write!(f, ">"),
            LogicOperation::GreaterThanEqual => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logic {
    pub lhs: Box<Expression>,
    pub op: LogicOperation,
    pub rhs: Box<Expression>,
}
