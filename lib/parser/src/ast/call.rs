use span_util::Span;

use super::{Expression, Identifier};

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expression>,
    pub arguments: Vec<CallArgs>,
    pub span: Span,
}


#[derive(Debug, PartialEq, Clone)]
pub struct CallArgs(pub Option<Identifier>, pub Expression);

impl CallArgs {
    pub fn is_named(&self) -> bool {
        match self.0 {
            Some(_) => true,
            _ => false
        }
    }
}