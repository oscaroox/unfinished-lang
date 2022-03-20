use span_util::Span;

use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
    pub expr: Box<Expression>,
    pub span: Span,
}
