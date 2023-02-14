use crate::scanner::Token;
use ariadne::{Label, Report, ReportKind};
use span_util::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParserError {
    #[error("{0}")]
    ExpectedToken(String, Token),

    #[error("{0}")]
    Error(String, Token),

    #[error("Unexpected token {0}")]
    UnexpectedToken(Token),

    #[error("Unterminated string")]
    UnterminatedString(Token),

    #[error("Expected parameter")]
    ExpectedParameter(Span),

    #[error("Unterminated string interpolation")]
    UnterminatedInterpolation(Token),

    #[error("Unterminated function call")]
    UnterminatedFunctionCall(Token),

    #[error("Invalid assignment target")]
    InvalidAssignmentTarget(Token),

    #[error("Invalid unary operation")]
    InvalidUnaryOperation(Token),

    #[error("Invalid use of 'unit' type, cannot use 'unit' here as type")]
    InvalidUseOfUnitType(Token),

    #[error("Invalid type '{0}'")]
    InvalidType(Token),
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
            | ParserError::InvalidUnaryOperation(tok)
            | ParserError::InvalidUseOfUnitType(tok)
            | ParserError::InvalidType(tok)
            | ParserError::UnterminatedString(tok) => {
                let label = Label::new(tok.span.to_range())
                    .with_message(msg);
                self.build_report(label)
            },
            ParserError::ExpectedParameter(span) => {
                let label = Label::new(span.to_range())
                    .with_message(msg);
                self.build_report(label)
            }
        }
    }

    fn build_report(&mut self, label: Label) -> Report {
        Report::build(ReportKind::Error, (), 99)
        .with_message("Parser Error")
        .with_label(label)
        .finish()
    }
}
