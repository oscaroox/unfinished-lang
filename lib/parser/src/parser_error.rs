use scanner::Token;
use spanner::SpannedError;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("{0}, found '{1}'")]
    ExpectedToken(String, Token),
    #[error("{0} {1}")]
    Error(String, Token),
    #[error("Unexpected token: {0}")]
    UnexpectedToken(Token),
}

impl ParserError {
    pub fn into_spanned_error(&mut self) -> SpannedError {
        let msg = self.to_string();
        match self {
            ParserError::ExpectedToken(_, tok)
            | ParserError::UnexpectedToken(tok)
            | ParserError::Error(_, tok) => SpannedError::new1(msg, tok.span),
        }
    }
}
