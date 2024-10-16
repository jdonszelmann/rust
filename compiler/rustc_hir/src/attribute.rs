use std::fmt::Display;

use rustc_ast::attr::AttributeExt;
use rustc_ast::token::CommentKind;
use rustc_ast::{self as ast, AttrId, AttrStyle, DelimArgs, MetaItemInner, MetaItemLit};
use rustc_macros::{Decodable, Encodable, HashStable_Generic};
use rustc_span::symbol::Ident;
use rustc_span::{Span, Symbol, sym};
use rustc_target::abi::Align;
use smallvec::SmallVec;
use thin_vec::ThinVec;

use crate::{RustcVersion, Safety};

#[derive(Copy, Clone, PartialEq, Encodable, Decodable, Debug, HashStable_Generic)]
pub enum InlineAttr {
    None,
    Hint,
    Always,
    Never,
}

#[derive(Clone, Encodable, Decodable, Debug, PartialEq, Eq, HashStable_Generic)]
pub enum InstructionSetAttr {
    ArmA32,
    ArmT32,
}

#[derive(Clone, Encodable, Decodable, Debug, HashStable_Generic)]
pub enum OptimizeAttr {
    None,
    Speed,
    Size,
}

#[derive(Clone, Debug, HashStable_Generic, Encodable, Decodable)]
pub enum DiagnosticAttribute {
    // tidy-alphabetical-start
    DoNotRecommend,
    OnUnimplemented,
    // tidy-alphabetical-end
}

#[derive(PartialEq, Debug, Encodable, Decodable, Copy, Clone, HashStable_Generic)]
pub enum Repr {
    Int(IntType),
    Rust,
    C,
    Packed(Align),
    Simd,
    Transparent,
    Align(Align),
}

pub enum TransparencyError {
    UnknownTransparency(Symbol, Span),
    MultipleTransparencyAttrs(Span, Span),
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
#[derive(Encodable, Decodable, HashStable_Generic)]
pub enum IntType {
    SignedInt(ast::IntTy),
    UnsignedInt(ast::UintTy),
}

impl IntType {
    #[inline]
    pub fn is_signed(self) -> bool {
        use IntType::*;

        match self {
            SignedInt(..) => true,
            UnsignedInt(..) => false,
        }
    }
}

#[derive(Copy, Debug, Encodable, Decodable, Clone, HashStable_Generic)]
pub struct Deprecation {
    pub since: DeprecatedSince,
    /// The note to issue a reason.
    pub note: Option<Symbol>,
    /// A text snippet used to completely replace any use of the deprecated item in an expression.
    ///
    /// This is currently unstable.
    pub suggestion: Option<Symbol>,
}

/// Release in which an API is deprecated.
#[derive(Copy, Debug, Encodable, Decodable, Clone, HashStable_Generic)]
pub enum DeprecatedSince {
    RustcVersion(RustcVersion),
    /// Deprecated in the future ("to be determined").
    Future,
    /// `feature(staged_api)` is off. Deprecation versions outside the standard
    /// library are allowed to be arbitrary strings, for better or worse.
    NonStandard(Symbol),
    /// Deprecation version is unspecified but optional.
    Unspecified,
    /// Failed to parse a deprecation version, or the deprecation version is
    /// unspecified and required. An error has already been emitted.
    Err,
}

impl Deprecation {
    /// Whether an item marked with #[deprecated(since = "X")] is currently
    /// deprecated (i.e., whether X is not greater than the current rustc
    /// version).
    pub fn is_in_effect(&self) -> bool {
        match self.since {
            DeprecatedSince::RustcVersion(since) => since <= RustcVersion::CURRENT,
            DeprecatedSince::Future => false,
            // The `since` field doesn't have semantic purpose without `#![staged_api]`.
            DeprecatedSince::NonStandard(_) => true,
            // Assume deprecation is in effect if "since" field is absent or invalid.
            DeprecatedSince::Unspecified | DeprecatedSince::Err => true,
        }
    }

    pub fn is_since_rustc_version(&self) -> bool {
        matches!(self.since, DeprecatedSince::RustcVersion(_))
    }
}

/// Arguments passed to an attribute macro.
#[derive(Clone, Debug, HashStable_Generic, Encodable, Decodable)]
pub enum AttrArgs {
    /// No arguments: `#[attr]`.
    Empty,
    /// Delimited arguments: `#[attr()/[]/{}]`.
    Delimited(DelimArgs),
    /// Arguments of a key-value attribute: `#[attr = "value"]`.
    Eq {
        /// Span of the `=` token.
        eq_span: Span,
        /// The "value".
        expr: MetaItemLit,
    },
}

#[derive(Clone, Debug, Encodable, Decodable)]
pub enum AttrKind {
    /// A normal attribute.
    Normal(Box<AttrItem>),

    /// A doc comment (e.g. `/// ...`, `//! ...`, `/** ... */`, `/*! ... */`).
    /// Doc attributes (e.g. `#[doc="..."]`) are represented with the `Normal`
    /// variant (which is much less compact and thus more expensive).
    DocComment(CommentKind, Symbol),
}

#[derive(Clone, Debug, HashStable_Generic, Encodable, Decodable)]
pub struct AttrPath {
    pub segments: Box<[Ident]>,
    pub span: Span,
}

impl AttrPath {
    pub fn from_ast(path: &ast::Path) -> Self {
        AttrPath {
            segments: path.segments.iter().map(|i| i.ident).collect::<Vec<_>>().into_boxed_slice(),
            span: path.span,
        }
    }
}

impl Display for AttrPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.segments.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("::"))
    }
}

#[derive(Clone, Debug, HashStable_Generic, Encodable, Decodable)]
pub struct AttrItem {
    pub unsafety: Safety,
    // Not lowered to hir::Path because we have no NodeId to resolve to.
    pub path: AttrPath,
    pub args: AttrArgs,
}

#[derive(Clone, Debug, Encodable, Decodable)]
pub struct Attribute {
    pub kind: AttrKind,
    pub id: AttrId,
    /// Denotes if the attribute decorates the following construct (outer)
    /// or the construct this attribute is contained within (inner).
    pub style: AttrStyle,
    pub span: Span,
}

impl Attribute {
    pub fn get_normal_item(&self) -> &AttrItem {
        match &self.kind {
            AttrKind::Normal(normal) => &normal,
            AttrKind::DocComment(..) => panic!("unexpected doc comment"),
        }
    }

    pub fn unwrap_normal_item(self) -> AttrItem {
        match self.kind {
            AttrKind::Normal(normal) => *normal,
            AttrKind::DocComment(..) => panic!("unexpected doc comment"),
        }
    }

    pub fn value_lit(&self) -> Option<&MetaItemLit> {
        match &self.kind {
            AttrKind::Normal(n) => match n.as_ref() {
                AttrItem { args: AttrArgs::Eq { expr, .. }, .. } => Some(expr),
                _ => None,
            },
            _ => None,
        }
    }
}

impl AttributeExt for Attribute {
    fn id(&self) -> AttrId {
        self.id
    }

    fn meta_item_list(&self) -> Option<ThinVec<ast::MetaItemInner>> {
        match &self.kind {
            AttrKind::Normal(n) => match n.as_ref() {
                AttrItem { args: AttrArgs::Delimited(d), .. } => {
                    ast::MetaItemKind::list_from_tokens(d.tokens.clone())
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn value_str(&self) -> Option<Symbol> {
        self.value_lit().and_then(|x| x.value_str())
    }

    fn value_span(&self) -> Option<Span> {
        self.value_lit().map(|i| i.span)
    }

    /// For a single-segment attribute, returns its name; otherwise, returns `None`.
    fn ident(&self) -> Option<Ident> {
        match &self.kind {
            AttrKind::Normal(n) => {
                if let [ident] = n.path.segments.as_ref() {
                    Some(*ident)
                } else {
                    None
                }
            }
            AttrKind::DocComment(..) => None,
        }
    }

    fn path_matches(&self, name: &[Symbol]) -> bool {
        match &self.kind {
            AttrKind::Normal(n) => {
                n.path.segments.len() == name.len()
                    && n.path.segments.iter().zip(name).all(|(s, n)| s.name == *n)
            }
            AttrKind::DocComment(..) => false,
        }
    }

    fn is_doc_comment(&self) -> bool {
        matches!(self.kind, AttrKind::DocComment(..))
    }

    fn span(&self) -> Span {
        self.span
    }

    fn is_word(&self) -> bool {
        match &self.kind {
            AttrKind::Normal(n) => {
                matches!(n.args, AttrArgs::Empty)
            }
            AttrKind::DocComment(..) => false,
        }
    }

    fn ident_path(&self) -> Option<SmallVec<[Ident; 1]>> {
        match &self.kind {
            AttrKind::Normal(n) => Some(n.path.segments.iter().copied().collect()),
            AttrKind::DocComment(..) => None,
        }
    }

    fn doc_str(&self) -> Option<Symbol> {
        match &self.kind {
            AttrKind::DocComment(.., data) => Some(*data),
            AttrKind::Normal(_) if self.has_name(sym::doc) => self.value_str(),
            _ => None,
        }
    }
    fn doc_str_and_comment_kind(&self) -> Option<(Symbol, CommentKind)> {
        match &self.kind {
            AttrKind::DocComment(kind, data) => Some((*data, *kind)),
            AttrKind::Normal(_) if self.name_or_empty() == sym::doc => {
                self.value_str().map(|s| (s, CommentKind::Line))
            }
            _ => None,
        }
    }

    fn style(&self) -> AttrStyle {
        self.style
    }
}

// FIXME(fn_delegation): use function delegation instead of manually forwarding
impl Attribute {
    pub fn id(&self) -> AttrId {
        AttributeExt::id(self)
    }

    pub fn name_or_empty(&self) -> Symbol {
        AttributeExt::name_or_empty(self)
    }

    pub fn meta_item_list(&self) -> Option<ThinVec<MetaItemInner>> {
        AttributeExt::meta_item_list(self)
    }

    pub fn value_str(&self) -> Option<Symbol> {
        AttributeExt::value_str(self)
    }

    pub fn value_span(&self) -> Option<Span> {
        AttributeExt::value_span(self)
    }

    pub fn ident(&self) -> Option<Ident> {
        AttributeExt::ident(self)
    }

    pub fn path_matches(&self, name: &[Symbol]) -> bool {
        AttributeExt::path_matches(self, name)
    }

    pub fn is_doc_comment(&self) -> bool {
        AttributeExt::is_doc_comment(self)
    }

    #[inline]
    pub fn has_name(&self, name: Symbol) -> bool {
        AttributeExt::has_name(self, name)
    }

    pub fn span(&self) -> Span {
        AttributeExt::span(self)
    }

    pub fn is_word(&self) -> bool {
        AttributeExt::is_word(self)
    }

    pub fn path(&self) -> SmallVec<[Symbol; 1]> {
        AttributeExt::path(self)
    }

    pub fn ident_path(&self) -> Option<SmallVec<[Ident; 1]>> {
        AttributeExt::ident_path(self)
    }

    pub fn doc_str(&self) -> Option<Symbol> {
        AttributeExt::doc_str(self)
    }

    pub fn is_proc_macro_attr(&self) -> bool {
        AttributeExt::is_proc_macro_attr(self)
    }

    pub fn doc_str_and_comment_kind(&self) -> Option<(Symbol, CommentKind)> {
        AttributeExt::doc_str_and_comment_kind(self)
    }

    pub fn style(&self) -> AttrStyle {
        AttributeExt::style(self)
    }
}
