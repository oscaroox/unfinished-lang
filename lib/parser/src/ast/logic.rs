use crate::scanner::{Token, TokenType};
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

impl LogicOperation {
    pub fn from_token(token: Token) -> LogicOperation {
        match token.token_type {
            TokenType::And => LogicOperation::And(token.span),
            TokenType::Or => LogicOperation::Or(token.span),
            TokenType::LessThan => LogicOperation::LessThan(token.span),
            TokenType::LessThanEqual => LogicOperation::LessThanEqual(token.span),
            TokenType::GreaterThan => LogicOperation::GreaterThan(token.span),
            TokenType::GreaterThanEqual => LogicOperation::GreaterThanEqual(token.span),
            TokenType::EqualEqual => LogicOperation::Equal(token.span),
            TokenType::NotEqual => LogicOperation::NotEqual(token.span),

            _ => panic!(
                "Cannot convert from token type: {} to logic operation",
                token.token_type
            ),
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
