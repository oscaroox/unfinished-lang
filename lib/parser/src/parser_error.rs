use ariadne::{Label, Report, ReportKind};
use scanner::TokenWithSpan;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("{0}")]
    ExpectedToken(String, TokenWithSpan),

    #[error("{0}")]
    Error(String, TokenWithSpan),

    #[error("Unexpected token")]
    UnexpectedToken(TokenWithSpan),

    #[error("Unterminated string")]
    UnterminatedString(TokenWithSpan),

    #[error("Unterminated string interpolation")]
    UnterminatedInterpolation(TokenWithSpan),

    #[error("Unterminated function call")]
    UnterminatedFunctionCall(TokenWithSpan),

    #[error("Invalid assignemnt target")]
    InvalidAssignmentTarget(TokenWithSpan),
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
                let label = Label::new(tok.1.to_range());
                Report::build(ReportKind::Error, (), 99)
                    .with_message("Parser Error")
                    .with_label(label.with_message(msg))
                    .finish()
            }
        }
    }
}
