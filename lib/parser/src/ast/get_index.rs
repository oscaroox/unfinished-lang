use span_util::Span;

use super::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct GetIndex {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
}
