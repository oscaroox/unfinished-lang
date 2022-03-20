use ariadne::{Label, Report, ReportKind};
use span_util::Span;
use thiserror::Error;

use type_core::Type;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum TypeError {
    #[error("Type Annotation needed, cannot infer type")]
    AnnotationNeeded(Span),

    #[error("Expected '{0}', got '{1}'")]
    Expected(Type, Type, Span),

    #[error("Expected .., got ..")]
    NotSubtype,

    #[error("Reference to undefined variable '{0}'")]
    UndefinedVariable(String, Span),

    #[error("Invalid unary operation '{0}' on type '{1}'")]
    InvalidUnaryOperation(String, Type, Span),

    #[error("Mismatched types expected {0}, found {1}")]
    MismatchedTypes(Type, Type, Span),

    #[error("Cannot index '{0}', can only index 'array'")]
    CannotIndex(Type, Span),

    #[error("Cannot index array with '{0}'")]
    CannotIndexByType(Type, Span),

    #[error("Cannot call '{0}'")]
    NotCallable(Type, Span),

    #[error("Missing property '{0}'")]
    MissingProperty(String, Span),

    #[error("Extra property '{0}'")]
    ExtraProperty(String, Span),

    #[error("Can only instantiate data struct")]
    InvalidDataStructInstantiation(Span),

    #[error("Unknown property '{0}'")]
    UnknownProperty(String, Span),

    #[error("Cannot access property from type '{0}'")]
    InvalidPropertyAccess(Type, Span),

    #[error("Cannot apply operation '{0}' to type '{1}' and '{2}'")]
    InvalidBinaryOperation(String, Type, Type, Span),

    #[error("Branches have incompatible types, expected '{0}', found '{1}'")]
    BranchesIncompatibleTypes(Type, Type, Span),

    #[error("Can only iterate type 'array', found '{0}'")]
    CannotIterate(Type, Span),

    #[error("This function takes {0} arguments but {1} arguments were supplied")]
    ExpectedArguments(usize, usize, Span),

    #[error("Cant compare '{0}' with '{1}'")]
    CantCompare(Type, Type, Span),

    #[error("Cannot call static method from instance")]
    CannotCallStaticMethod(Span),

    #[error("")]
    LeftAndRight(Box<TypeError>, Box<TypeError>),
}

impl TypeError {
    pub fn into_report(&mut self) -> Report {
        let msg = self.to_string();

        match self {
            TypeError::UndefinedVariable(_, span)
            | TypeError::CannotIndex(_, span)
            | TypeError::CannotIterate(_, span)
            | TypeError::AnnotationNeeded(span)
            | TypeError::CannotIndexByType(_, span)
            | TypeError::NotCallable(_, span)
            | TypeError::MissingProperty(_, span)
            | TypeError::InvalidUnaryOperation(_, _, span)
            | TypeError::ExtraProperty(_, span)
            | TypeError::InvalidDataStructInstantiation(span)
            | TypeError::UnknownProperty(_, span)
            | TypeError::InvalidPropertyAccess(_, span)
            | TypeError::Expected(_, _, span)
            | TypeError::ExpectedArguments(_, _, span)
            | TypeError::BranchesIncompatibleTypes(_, _, span)
            | TypeError::CantCompare(_, _, span)
            | TypeError::CannotCallStaticMethod(span)
            | TypeError::MismatchedTypes(_, _, span) => {
                let label = Label::new(span.to_range());
                Report::build(ReportKind::Error, (), 99)
                    .with_message("TypeError")
                    .with_label(label.with_message(msg))
                    .finish()
            }
            TypeError::InvalidBinaryOperation(_, _, _, span) => {
                let label = Label::new(span.to_range());
                Report::build(ReportKind::Error, (), 99)
                    .with_message(format!("TypeError: Invalid binary operation"))
                    .with_label(label.with_message(msg))
                    .finish()
            }
            _ => todo!(),
        }
    }
}
