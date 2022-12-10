use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct IfConditional {
    pub condition: Box<Expression>,
    pub then: Box<Expression>,
    pub not_then: Option<Box<Expression>>,
    pub span: Span,
    pub if_token: Span,
}
