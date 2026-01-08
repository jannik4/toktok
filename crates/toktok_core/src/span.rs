use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Span {
    Eoi,
    Range(Range<usize>),
}

impl From<Range<usize>> for Span {
    fn from(v: Range<usize>) -> Self {
        Self::Range(v)
    }
}

impl Span {
    pub fn as_range(&self) -> Option<&Range<usize>> {
        if let Self::Range(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
