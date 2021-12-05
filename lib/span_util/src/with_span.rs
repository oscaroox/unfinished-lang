use crate::Span;

#[derive(Debug, Clone)]
pub struct WithSpan<T>(pub T, pub Span);

impl<T: PartialEq> Eq for WithSpan<T> {}

impl<T: PartialEq> PartialEq for WithSpan<T> {
    fn eq(&self, other: &Self) -> bool {
        // Ignore the span when comparing expressions
        // this is only usefull for tests
        self.0 == other.0
    }
}
