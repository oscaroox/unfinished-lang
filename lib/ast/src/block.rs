use span_util::Span;

use crate::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub exprs: Vec<Expression>,
    pub span: Span,
}
