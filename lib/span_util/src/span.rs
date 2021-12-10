use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn fake() -> Span {
        Span { start: 0, end: 0 }
    }

    pub fn to_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn extend(&self, span: Span) -> Span {
        Span {
            start: self.start,
            end: span.end,
        }
    }
}

impl Eq for Span {}

impl PartialEq for Span {
    fn eq(&self, _: &Self) -> bool {
        // Ignore the span when comparing expressions
        // this is only usefull for tests
        true
    }
}

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Self {
        Self {
            start: r.start,
            end: r.end,
        }
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        Range {
            start: self.start,
            end: self.end,
        }
    }
}
