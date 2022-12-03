use span_util::Span;

use crate::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expression>,
    pub arguments: Vec<CallArgs>,
    pub span: Span,
}


#[derive(Debug, PartialEq, Clone)]
pub struct CallArgs(pub Option<Identifier>, pub Expression);