use crate::Span;

#[derive(Debug, Clone)]
pub struct WithSpan<T>(pub T, pub Span);

impl<T> WithSpan<T> {
    pub fn new(value: T, span: Span) -> WithSpan<T> {
        WithSpan(value, span)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for WithSpan<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: PartialEq> Eq for WithSpan<T> {}

impl<T: PartialEq> PartialEq for WithSpan<T> {
    fn eq(&self, other: &Self) -> bool {
        // Ignore the span when comparing expressions
        // this is only usefull for tests
        self.0 == other.0
    }
}
