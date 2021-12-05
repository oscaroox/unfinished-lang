use scanner::{Token, TokenType};

use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperation {
    Add,
    // TODO while this is convenient, this should be its own expression
    ConcatInterpolation,
    Substract,
    Multiply,
    Divide,
}

impl BinaryOperation {
    pub fn from_token(token: Token) -> BinaryOperation {
        match token.token_type {
            TokenType::Plus | TokenType::AssignPlus => BinaryOperation::Add,
            TokenType::Minus | TokenType::AssignMinus => BinaryOperation::Substract,
            TokenType::Star | TokenType::AssignStar => BinaryOperation::Multiply,
            TokenType::Slash | TokenType::AssignSlash => BinaryOperation::Divide,
            _ => panic!(
                "Cannot convert from tokentype: {} to binary operation",
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
}
