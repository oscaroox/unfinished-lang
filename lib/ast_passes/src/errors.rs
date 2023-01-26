use ariadne::{Label, Report, ReportKind};
use span_util::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum PassesError {
    // name resolver errors
    #[error("cannot find value '{0}'")]
    CannotFindValue(String, Span),

    // semantic analyzer errors
    #[error("Cannot use return in top-level code")]
    NoTopLevelReturn(Span),
    #[error("Cannot use break outside loop expression")]
    NoBreakOutsideLoop(Span),
    #[error("Cannot use continue outside loop expression")]
    NoContinueOutsideLoop(Span),
    #[error("Cannot use self outside methods")]
    NoSelfOutsideMethod(Span),
    #[error("Cannot use named arguments with positional arguments")]
    NoUsePositionalWithNamedArgs(Span),
    #[error("Type annotation needed")]
    TypeAnnotationNeeded(Span),
}

impl PassesError {
    pub fn into_report(&mut self) -> Report {
        let msg = self.to_string();
        match self {
            Self::CannotFindValue(_, span)
            | Self::NoBreakOutsideLoop(span)
            | Self::NoContinueOutsideLoop(span)
            | Self::NoSelfOutsideMethod(span)
            | Self::NoUsePositionalWithNamedArgs(span)
            | Self::NoTopLevelReturn(span)
            | Self::TypeAnnotationNeeded(span) => {
                let label = Label::new(span.to_range());
                Report::build(ReportKind::Error, (), 99)
                    .with_message("Parser Error")
                    .with_label(label.with_message(msg))
                    .finish()
            }
        }
    }
}
