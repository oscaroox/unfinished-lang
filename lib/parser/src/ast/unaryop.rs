use crate::scanner::{Token, TokenType};
use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperation {
    Minus(Span),
    Plus(Span),
    Not(Span),
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperation::Minus(_) => write!(f, "-"),
            UnaryOperation::Plus(_) => write!(f, "+"),
            UnaryOperation::Not(_) => write!(f, "!"),
        }
    }
}

impl UnaryOperation {
    pub fn from_token(token: Token) -> UnaryOperation {
        match token.token_type {
            TokenType::Plus => UnaryOperation::Plus(token.span),
            TokenType::Minus => UnaryOperation::Minus(token.span),
            TokenType::Bang => UnaryOperation::Not(token.span),
            _ => panic!(
                "Cannot convert from token type: {} to unary operation",
                token.token_type
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperation,
    pub rhs: Box<Expression>,
    pub span: Span,
}
