use span_util::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct SelfExpr {
    pub name: String,
    pub span: Span,
}
