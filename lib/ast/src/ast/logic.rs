use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum LogicOperation {
    Or(Span),
    And(Span),
    Equal(Span),
    NotEqual(Span),
    LessThan(Span),
    LessThanEqual(Span),
    GreaterThan(Span),
    GreaterThanEqual(Span),
}

impl std::fmt::Display for LogicOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicOperation::Or(_) => write!(f, "||"),
            LogicOperation::And(_) => write!(f, "&&"),
            LogicOperation::Equal(_) => write!(f, "=="),
            LogicOperation::NotEqual(_) => write!(f, "!="),
            LogicOperation::LessThan(_) => write!(f, "<"),
            LogicOperation::LessThanEqual(_) => write!(f, "<="),
            LogicOperation::GreaterThan(_) => write!(f, ">"),
            LogicOperation::GreaterThanEqual(_) => write!(f, ">="),
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct Logic {
    pub lhs: Box<Expression>,
    pub op: LogicOperation,
    pub rhs: Box<Expression>,
    pub span: Span,
}
