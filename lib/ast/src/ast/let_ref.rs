use span_util::Span;

use super::Identifier;

#[derive(Debug, PartialEq, Clone)]
pub struct LetRef {
    pub name: Identifier,
    pub scope_distance: Option<usize>,
    pub span: Span,
}
