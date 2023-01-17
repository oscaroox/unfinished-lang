use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ImplicitReturn {
    pub value: Box<Expression>,
    pub span: Span,
}
