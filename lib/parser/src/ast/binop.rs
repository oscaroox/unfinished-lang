
use span_util::Span;

use crate::scanner::{Token, TokenType};

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperation {
    Add(Span),
    // TODO while this is convenient, this should be its own expression
    ConcatInterpolation,
    Subtract(Span),
    Multiply(Span),
    Divide(Span),
}

impl std::fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperation::Add(_) => write!(f, "+"),
            BinaryOperation::ConcatInterpolation => write!(f, ""),
            BinaryOperation::Subtract(_) => write!(f, "-"),
            BinaryOperation::Multiply(_) => write!(f, "*"),
            BinaryOperation::Divide(_) => write!(f, "/"),
        }
    }
}

impl BinaryOperation {
    pub fn from_token(token: Token) -> BinaryOperation {
        match token.token_type {
            TokenType::Plus | TokenType::AssignPlus => BinaryOperation::Add(token.span),
            TokenType::Minus | TokenType::AssignMinus => BinaryOperation::Subtract(token.span),
            TokenType::Star | TokenType::AssignStar => BinaryOperation::Multiply(token.span),
            TokenType::Slash | TokenType::AssignSlash => BinaryOperation::Divide(token.span),
            _ => panic!(
                "Cannot convert from token type: {} to binary operation",
                token.token_type
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinOp {
    pub left: Box<Expression>,
    pub op: BinaryOperation,
    pub right: Box<Expression>,
    pub span: Span,
}