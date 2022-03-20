use span_util::Span;

use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ImplicitReturn {
    pub value: Box<Expression>,
    pub span: Span,
}
