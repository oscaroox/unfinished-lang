use span_util::Span;

use crate::{Expression, Identifier};

#[derive(Debug, Clone, PartialEq)]
pub struct SetProperty {
    pub object: Box<Expression>,
    pub name: Identifier,
    pub value: Box<Expression>,
    pub span: Span,
}
