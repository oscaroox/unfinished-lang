use span_util::Span;

use crate::Identifier;

#[derive(Debug, PartialEq, Clone)]
pub struct LetRef {
    pub name: Identifier,
    pub span: Span,
}
