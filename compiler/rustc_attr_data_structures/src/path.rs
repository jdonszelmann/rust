use std::fmt;

use rustc_ast::{self as ast};
use rustc_macros::{Decodable, Encodable, HashStable_Generic};
use rustc_span::{Ident, Span};

#[derive(Clone, Debug, HashStable_Generic, Encodable, Decodable)]
pub struct AttrPath {
    pub segments: Box<[Ident]>,
    pub span: Span,
}

impl AttrPath {
    fn as_borrowed<'a>(&'a self) -> BorrowedAttrPath<'a> {
        BorrowedAttrPath { segments: &self.segments }
    }
}

#[derive(Clone, Debug)]
pub struct BorrowedAttrPath<'a> {
    pub segments: &'a [Ident],
}

impl AttrPath {
    pub fn from_ast(path: &ast::Path) -> Self {
        AttrPath {
            segments: path.segments.iter().map(|i| i.ident).collect::<Vec<_>>().into_boxed_slice(),
            span: path.span,
        }
    }
}

impl fmt::Display for BorrowedAttrPath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.segments.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("::"))
    }
}

impl fmt::Display for AttrPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_borrowed().fmt(f)
    }
}
