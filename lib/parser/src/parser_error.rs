use scanner::{Position, Token};
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("{0}")]
    ExpectedToken(String),
    #[error("Unexpected token: {0}")]
    UnexpectedToken(Token),
    #[error("Expected expression after {token} at {pos} ")]
    ExpectedExpression { token: Token, pos: Position },
}
