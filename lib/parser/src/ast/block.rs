use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub exprs: Vec<Expression>,
    pub span: Span,
}
