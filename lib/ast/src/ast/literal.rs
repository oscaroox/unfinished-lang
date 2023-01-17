use span_util::Span;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<Expression>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub value: LiteralValue,
    pub span: Span,
}
