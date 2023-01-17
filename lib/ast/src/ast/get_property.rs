use span_util::Span;

use super::{Expression, Identifier};

#[derive(Debug, Clone, PartialEq)]
pub struct GetProperty {
    pub object: Box<Expression>,
    pub name: Identifier,
    pub is_callable: bool,
    pub span: Span,
}
