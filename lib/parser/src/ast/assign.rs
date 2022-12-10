use span_util::Span;

use super::{Expression, Identifier};



#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub name: Identifier,
    pub rhs: Box<Expression>,
    pub span: Span,
}
