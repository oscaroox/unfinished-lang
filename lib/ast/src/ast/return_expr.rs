use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnExpr {
    pub value: Box<Option<Expression>>,
    pub return_token: Span,
    pub span: Span,
}
