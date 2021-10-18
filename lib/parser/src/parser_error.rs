use scanner::Token;
use spanner::SpannedError;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("{0}, {1}")]
    ExpectedToken(String, Token),
    #[error("Unexpected token: {0}")]
    UnexpectedToken(Token),
    #[error("Expected expression after")]
    ExpectedExpression(Token),
}

impl ParserError {
    pub fn into_spanned_error(&mut self) -> SpannedError {
        let msg = self.to_string();
        match self {
            ParserError::ExpectedToken(_, tok)
            | ParserError::ExpectedExpression(tok)
            | ParserError::UnexpectedToken(tok) => SpannedError::new1(msg, tok.span),
        }
    }
}
