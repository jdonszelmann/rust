use rustc_macros::HashStable_Generic;
use rustc_span::{Span, Symbol};

#[derive(Clone, Debug, HashStable_Generic)]
pub struct AttributeLint<Id> {
    pub id: Id,
    pub span: Span,
    pub kind: AttributeLintKind,
}

#[derive(Copy, Clone, Debug, HashStable_Generic)]
pub enum UnusedNote {
    EmptyList { name: Symbol },
    NoLints { name: Symbol },
}

#[derive(Clone, Debug, HashStable_Generic)]
pub enum AttributeLintKind {
    UnusedDuplicate { this: Span, other: Span, warning: bool },
    IllFormedAttributeInput { suggestions: Vec<String> },
    Unused { first_span: Span, note: UnusedNote },
}
