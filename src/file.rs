use derive_new::new;
#[derive(Debug, PartialEq, new)]
pub struct Locatable<T> {
    pub location: Span,
    pub value: T,
}
#[derive(Debug, PartialEq, new)]
pub struct Span {
    start: usize,
    end: usize,
}
