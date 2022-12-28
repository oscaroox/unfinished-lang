use span_util::Span;

use super::{Expression, Identifier};
use type_core::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub params: Vec<Identifier>,
    pub return_type: Type,
    pub body: Box<Expression>,
    pub is_static: bool,
    pub span: Span,
}
