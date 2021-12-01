use ariadne::{Label, Report, ReportKind};
use scanner::{Token, TokenWithLabel};
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("{0}")]
    ExpectedToken(String, TokenWithLabel),

    #[error("{0}")]
    Error(String, TokenWithLabel),

    #[error("Unexpected token")]
    UnexpectedToken(TokenWithLabel),

    #[error("Unterminated string")]
    UnterminatedString(TokenWithLabel),

    #[error("Unterminated string interpolation")]
    UnterminatedInterpolation(TokenWithLabel),

    #[error("Unterminated function call")]
    UnterminatedFunctionCall(TokenWithLabel),

    #[error("Invalid assignemnt target")]
    InvalidAssignmentTarget(TokenWithLabel),
}

impl ParserError {
    pub fn into_report(&mut self) -> Report {
        let msg = self.to_string();
        match self {
            ParserError::ExpectedToken(_, tok)
            | ParserError::UnexpectedToken(tok)
            | ParserError::Error(_, tok)
            | ParserError::InvalidAssignmentTarget(tok)
            | ParserError::UnterminatedFunctionCall(tok)
            | ParserError::UnterminatedInterpolation(tok)
            | ParserError::UnterminatedString(tok) => {
                let label = Label::new(tok.1.clone());
                Report::build(ReportKind::Error, (), 99)
                    .with_message("Parser Error")
                    .with_label(label.with_message(msg))
                    .finish()
            }
        }
    }
}
