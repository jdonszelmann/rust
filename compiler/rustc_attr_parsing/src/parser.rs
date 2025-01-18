use core::panic;
use std::fmt::{Debug, Display};
use std::iter::Peekable;

use rustc_ast::token::{self, Delimiter, Token};
use rustc_ast::tokenstream::{TokenStreamIter, TokenTree};
use rustc_ast::{AttrArgs, DelimArgs, Expr, ExprKind, LitKind, MetaItemLit, NormalAttr, Path};
use rustc_ast_pretty::pprust;
use rustc_errors::DiagCtxtHandle;
use rustc_hir::{self as hir, AttrPath};
use rustc_span::symbol::{Ident, kw};
use rustc_span::{DUMMY_SP, Span, Symbol};

pub(crate) struct SegmentIterator<'a> {
    offset: usize,
    path: &'a PathParser<'a>,
}

impl<'a> Iterator for SegmentIterator<'a> {
    type Item = &'a Ident;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.path.len() {
            return None;
        }

        let res = match self.path {
            PathParser::Ast(ast_path) => &ast_path.segments[self.offset].ident,
            PathParser::Attr(attr_path) => &attr_path.segments[self.offset],
        };

        self.offset += 1;
        Some(res)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum PathParser<'a> {
    Ast(&'a Path),
    Attr(AttrPath),
}

impl<'a> PathParser<'a> {
    pub(crate) fn get_attribute_path(&self) -> hir::AttrPath {
        AttrPath {
            segments: self.segments().copied().collect::<Vec<_>>().into_boxed_slice(),
            span: self.span(),
        }
    }

    pub(crate) fn segments(&'a self) -> impl Iterator<Item = &'a Ident> {
        SegmentIterator { offset: 0, path: self }
    }

    pub(crate) fn span(&self) -> Span {
        match self {
            PathParser::Ast(path) => path.span,
            PathParser::Attr(attr_path) => attr_path.span,
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            PathParser::Ast(path) => path.segments.len(),
            PathParser::Attr(attr_path) => attr_path.segments.len(),
        }
    }

    pub(crate) fn segments_is(&self, segments: &[Symbol]) -> bool {
        self.len() == segments.len() && self.segments().zip(segments).all(|(a, b)| a.name == *b)
    }

    pub(crate) fn word(&self) -> Option<Ident> {
        (self.len() == 1).then(|| **self.segments().next().as_ref().unwrap())
    }

    pub(crate) fn word_or_empty(&self) -> Ident {
        self.word().unwrap_or_else(Ident::empty)
    }

    /// Asserts that this MetaItem is some specific word.
    ///
    /// See [`word`](Self::word) for examples of what a word is.
    pub(crate) fn word_is(&self, sym: Symbol) -> bool {
        self.word().map(|i| i.name == sym).unwrap_or(false)
    }
}

impl Display for PathParser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathParser::Ast(path) => write!(f, "{}", pprust::path_to_string(path)),
            PathParser::Attr(attr_path) => write!(f, "{attr_path}"),
        }
    }
}

#[derive(Clone, Debug)]
#[must_use]
pub(crate) enum ArgParser<'a> {
    NoArgs,
    List(MetaItemListParser<'a>),
    NameValue(NameValueParser),
}

impl<'a> ArgParser<'a> {
    pub(crate) fn span(&self) -> Option<Span> {
        match self {
            Self::NoArgs => None,
            Self::List(l) => Some(l.span),
            Self::NameValue(n) => Some(n.value_span.with_lo(n.eq_span.lo())),
        }
    }

    pub(crate) fn from_attr_args(value: &'a AttrArgs, dcx: DiagCtxtHandle<'a>) -> Self {
        match value {
            AttrArgs::Empty => Self::NoArgs,
            AttrArgs::Delimited(args) if args.delim == Delimiter::Parenthesis => Self::List(MetaItemListParser::new(args, dcx)),
            AttrArgs::Delimited(_) => todo!("nice diagnostic?"),
            AttrArgs::Eq { eq_span, expr } => Self::NameValue( NameValueParser {
                eq_span: *eq_span,
                value: expr_to_lit(dcx, &expr),
                value_span: expr.span,
            }),
        }
    }

    /// Asserts that this MetaItem is a list
    ///
    /// Some examples:
    ///
    /// - `#[allow(clippy::complexity)]`: `(clippy::complexity)` is a list
    /// - `#[rustfmt::skip::macros(target_macro_name)]`: `(target_macro_name)` is a list
    pub(crate) fn list(&self) -> Option<&MetaItemListParser<'a>> {
        match self {
            Self::List(l)  => Some(l),
            Self::NameValue(_) | Self::NoArgs => None,
        }
    }

    /// Asserts that this MetaItem is a name-value pair.
    ///
    /// Some examples:
    ///
    /// - `#[clippy::cyclomatic_complexity = "100"]`: `clippy::cyclomatic_complexity = "100"` is a name value pair,
    ///   where the name is a path (`clippy::cyclomatic_complexity`). You already checked the path
    ///   to get an `ArgParser`, so this method will effectively only assert that the `= "100"` is
    ///   there
    /// - `#[doc = "hello"]`: `doc = "hello`  is also a name value pair
    pub(crate) fn name_value(&self) -> Option<&NameValueParser> {
        match self {
            Self::NameValue(n) => Some(n),
            Self::List(_) | Self::NoArgs => None,
        }
    }

    /// Asserts that there are no arguments
    pub(crate) fn no_args(&self) -> bool {
        matches!(self, Self::NoArgs)
    }
}

/// Inside lists, values could be either literals, or more deeply nested meta items.
/// This enum represents that.
///
/// Choose which one you want using the provided methods.
#[derive(Debug, Clone)]
pub(crate) enum MetaItemOrLitParser<'a> {
    MetaItemParser(MetaItemParser<'a>),
    Lit(MetaItemLit),
}

impl<'a> MetaItemOrLitParser<'a> {
    pub(crate) fn span(&self) -> Span {
        match self {
            MetaItemOrLitParser::MetaItemParser(generic_meta_item_parser) => {
                generic_meta_item_parser.span()
            }
            MetaItemOrLitParser::Lit(meta_item_lit) => meta_item_lit.span,
        }
    }

    pub(crate) fn lit(&self) -> Option<&MetaItemLit> {
        match self {
            MetaItemOrLitParser::Lit(meta_item_lit) => Some(meta_item_lit),
            _ => None,
        }
    }

    pub(crate) fn meta_item(&self) -> Option<&MetaItemParser<'a>> {
        match self {
            MetaItemOrLitParser::MetaItemParser(parser) => Some(parser),
            _ => None,
        }
    }
}

/// Utility that deconstructs a MetaItem into usable parts.
///
/// MetaItems are syntactically extremely flexible, but specific attributes want to parse
/// them in custom, more restricted ways. This can be done using this struct.
///
/// MetaItems consist of some path, and some args. The args could be empty. In other words:
///
/// - `name` -> args are empty
/// - `name(...)` -> args are a [`list`](ArgParser::list), which is the bit between the parentheses
/// - `name = value`-> arg is [`name_value`](ArgParser::name_value), where the argument is the
///   `= value` part
///
/// The syntax of MetaItems can be found at <https://doc.rust-lang.org/reference/attributes.html>
#[derive(Clone)]
pub(crate) struct MetaItemParser<'a> {
    path: PathParser<'a>,
    args: ArgParser<'a>,
    dcx: DiagCtxtHandle<'a>,
}

impl<'a> Debug for MetaItemParser<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MetaItemParser")
            .field("path", &self.path)
            .field("args", &self.args)
            .finish()
    }
}

impl<'a> MetaItemParser<'a> {
    /// Create a new parser from a [`NormalAttr`], which is stored inside of any
    /// [`ast::Attribute`](Attribute)
    pub(crate) fn from_attr(attr: &'a NormalAttr, dcx: DiagCtxtHandle<'a>) -> Self {
        Self {
            path: PathParser::Ast(&attr.item.path),
            args: ArgParser::from_attr_args(&attr.item.args, dcx),
            dcx,
        }
    }
}

impl<'a> MetaItemParser<'a> {
    pub(crate) fn span(&self) -> Span {
        if let Some(other) = self.args.span() {
            self.path.span().with_hi(other.hi())
        } else {
            self.path.span()
        }
    }

    /// Gets just the path, without the args.
    pub(crate) fn path_without_args(&self) -> PathParser<'a> {
        self.path.clone()
    }

    /// Gets just the args parser, without caring about the path.
    pub(crate) fn args(&self) -> &ArgParser<'a> {
        &self.args
    }

    pub(crate) fn deconstruct(&self) -> (PathParser<'a>, &ArgParser<'a>) {
        (self.path_without_args(), self.args())
    }

    /// Asserts that this MetaItem starts with a path. Some examples:
    ///
    /// - `#[rustfmt::skip]`: `rustfmt::skip` is a path
    /// - `#[allow(clippy::complexity)]`: `clippy::complexity` is a path
    /// - `#[inline]`: `inline` is a single segment path
    /// - `#[inline(always)]`: `always` is a single segment path, but `inline` is *not and
    ///    should be parsed using [`list`](Self::list)
    pub(crate) fn path(&self) -> (PathParser<'a>, &ArgParser<'a>) {
        self.deconstruct()
    }

    /// Asserts that this MetaItem starts with a word, or single segment path.
    /// Doesn't return the args parser.
    ///
    /// For examples. see [`Self::word`]
    pub(crate) fn word_without_args(&self) -> Option<Ident> {
        Some(self.word()?.0)
    }

    /// Like [`word`](Self::word), but returns an empty symbol instead of None
    pub(crate) fn word_or_empty_without_args(&self) -> Ident {
        self.word_or_empty().0
    }

    /// Asserts that this MetaItem starts with a word, or single segment path.
    ///
    /// Some examples:
    /// - `#[inline]`: `inline` is a word
    /// - `#[rustfmt::skip]`: `rustfmt::skip` is a path, and not a word
    /// - `#[inline(always)]`: `always` is a word, but `inline` is *not and should be parsed
    ///   using [`path_list`](Self::path_list)
    /// - `#[allow(clippy::complexity)]`: `clippy::complexity` is *not* a word, and should instead be parsed
    ///   using [`path`](Self::path)
    pub(crate) fn word(&self) -> Option<(Ident, &ArgParser<'a>)> {
        let (path, args) = self.deconstruct();
        Some((path.word()?, args))
    }

    /// Like [`word`](Self::word), but returns an empty symbol instead of None
    pub(crate) fn word_or_empty(&self) -> (Ident, &ArgParser<'a>) {
        let (path, args) = self.deconstruct();
        (path.word().unwrap_or(Ident::empty()), args)
    }

    /// Asserts that this MetaItem starts with some specific word.
    ///
    /// See [`word`](Self::word) for examples of what a word is.
    pub(crate) fn word_is(&self, sym: Symbol) -> Option<&ArgParser<'a>> {
        self.path_without_args().word_is(sym).then(|| self.args())
    }

    /// Asserts that this MetaItem starts with some specific path.
    ///
    /// See [`word`](Self::path) for examples of what a word is.
    pub(crate) fn path_is(&self, segments: &[Symbol]) -> Option<&ArgParser<'a>> {
        self.path_without_args().segments_is(segments).then(|| self.args())
    }
}

#[derive(Clone)]
pub(crate) struct NameValueParser {
    pub(crate) eq_span: Span,
    value: MetaItemLit,
    pub(crate) value_span: Span,
}

impl Debug for NameValueParser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NameValueParser")
            .field("eq_span", &self.eq_span)
            .field("value", &self.value)
            .field("value_span", &self.value_span)
            .finish()
    }
}

impl NameValueParser {
    pub(crate) fn value_as_lit(&self) -> &MetaItemLit {
        &self.value
    }

    pub(crate) fn value_as_str(&self) -> Option<Symbol> {
        self.value_as_lit().kind.str()
    }
}

fn expr_to_lit(dcx: DiagCtxtHandle<'_>, expr: &Expr) -> MetaItemLit {
    // In valid code the value always ends up as a single literal. Otherwise, a dummy
    // literal suffices because the error is handled elsewhere.
    if let ExprKind::Lit(token_lit) = expr.kind
        && let Ok(lit) = MetaItemLit::from_token_lit(token_lit, expr.span)
    {
        lit
    } else {
        let guar = dcx.has_errors().unwrap();
        MetaItemLit { symbol: kw::Empty, suffix: None, kind: LitKind::Err(guar), span: DUMMY_SP }
    }
}

struct MetaItemListParserContext<'a> {
    // the tokens inside the delimiters, so `#[some::attr(a b c)]` would have `a b c` inside
    inside_delimiters: Peekable<TokenStreamIter<'a>>,
    dcx: DiagCtxtHandle<'a>,
}

impl<'a> MetaItemListParserContext<'a> {
    fn done(&mut self) -> bool {
        self.inside_delimiters.peek().is_none()
    }

    fn next_path(&mut self) -> AttrPath {
        // FIXME: Share code with `parse_path`.
        let tt = self.inside_delimiters.next().map(|tt| TokenTree::uninterpolate(tt));

        match tt.as_deref() {
            Some(&TokenTree::Token(
                Token { kind: ref kind @ (token::Ident(..) | token::PathSep), span },
                _,
            )) => {
                let mut segments = if let &token::Ident(name, _) = kind {
                    if let Some(TokenTree::Token(Token { kind: token::PathSep, .. }, _)) =
                        self.inside_delimiters.peek()
                    {
                        self.inside_delimiters.next();
                        vec![Ident::new(name, span)]
                    } else {
                        return AttrPath {
                            segments: vec![Ident::new(name, span)].into_boxed_slice(),
                            span,
                        };
                    }
                } else {
                    vec![Ident::new(kw::PathRoot, span)]
                };
                loop {
                    if let Some(&TokenTree::Token(Token { kind: token::Ident(name, _), span }, _)) =
                        self.inside_delimiters
                            .next()
                            .map(|tt| TokenTree::uninterpolate(tt))
                            .as_deref()
                    {
                        segments.push(Ident::new(name, span));
                    } else {
                        unreachable!()
                    }
                    if let Some(TokenTree::Token(Token { kind: token::PathSep, .. }, _)) =
                        self.inside_delimiters.peek()
                    {
                        self.inside_delimiters.next();
                    } else {
                        break;
                    }
                }
                let span = span.with_hi(segments.last().unwrap().span.hi());
                AttrPath { segments: segments.into_boxed_slice(), span }
            }
            Some(TokenTree::Token(
                Token { kind: token::OpenDelim(_) | token::CloseDelim(_), .. },
                _,
            )) => {
                panic!("Should be `AttrTokenTree::Delimited`, not delim tokens: {:?}", tt);
            }
            x => unreachable!("{x:?}"),
        }
    }

    fn value(&mut self) -> MetaItemLit {
        match self.inside_delimiters.next() {
            Some(TokenTree::Delimited(.., Delimiter::Invisible(_), inner_tokens)) => {
                MetaItemListParserContext {
                    inside_delimiters: inner_tokens.iter().peekable(),
                    dcx: self.dcx,
                }
                .value()
            }
            Some(TokenTree::Token(ref token, _)) => MetaItemLit::from_token(token).unwrap(),
            x => unreachable!("{x:?}"),
        }
    }

    fn next(&mut self) -> MetaItemOrLitParser<'a> {
        // a list element is either a literal
        if let Some(TokenTree::Token(token, _)) = self.inside_delimiters.peek()
            && let Some(lit) = MetaItemLit::from_token(token)
        {
            self.inside_delimiters.next();
            return MetaItemOrLitParser::Lit(lit);
        }

        // or a path.
        let path = if let Some(TokenTree::Token(Token { kind: token::Interpolated(nt), .. }, _)) = self.inside_delimiters.peek()
        {
            match &**nt {
                // or maybe a full nt meta including the path but we return immediately
                token::Nonterminal::NtMeta(item) => {
                    self.inside_delimiters.next();

                    return MetaItemOrLitParser::MetaItemParser(MetaItemParser {
                        path: PathParser::Ast(&item.path),
                        args: ArgParser::from_attr_args(&item.args, self.dcx),
                        dcx: self.dcx,
                    })
                }
                token::Nonterminal::NtPath(path) => {
                    self.inside_delimiters.next();


                    AttrPath::from_ast(path)
                }
                _ => {
                    unreachable!()
                }
            }
        } else {
            self.next_path()
        };



        // Paths can be followed by:
        // - `(more meta items)` (another list)
        // - `= lit` (a name-value)
        // - nothing
        MetaItemOrLitParser::MetaItemParser(match self.inside_delimiters.peek() {
            Some(TokenTree::Delimited(dspan, _, Delimiter::Parenthesis, inner_tokens)) => {
                self.inside_delimiters.next();

                MetaItemParser {
                    path: PathParser::Attr(path),
                    args: ArgParser::List(MetaItemListParser::new_tts(inner_tokens.iter(), dspan.entire(), self.dcx)),
                    dcx: self.dcx,
                }
            }
            // FIXME(jdonszelmann) nice error?
            // tests seem to say its already parsed and rejected maybe?
            Some(TokenTree::Delimited(..)) => {
                unreachable!()
            }
            Some(TokenTree::Token(Token { kind: token::Eq, span }, _)) => {
                self.inside_delimiters.next();
                let value = self.value();
                MetaItemParser {
                    path: PathParser::Attr(path),
                    args: ArgParser::NameValue(NameValueParser { eq_span: *span, value_span: value.span, value }),
                    dcx: self.dcx,
                }
            }
            _ => MetaItemParser { path: PathParser::Attr(path), args: ArgParser::NoArgs, dcx: self.dcx },
        })
    }

    pub(crate) fn parse(mut self, span: Span) -> MetaItemListParser<'a> {
        let mut sub_parsers = Vec::new();

        while !self.done() {
            sub_parsers.push(self.next());
            match self.inside_delimiters.next() {
                None | Some(TokenTree::Token(Token { kind: token::Comma, .. }, _)) => {}
                x => {
                    unreachable!("None returned: {x:?}");
                }
            }
        }

        MetaItemListParser { sub_parsers, span }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MetaItemListParser<'a> {
    sub_parsers: Vec<MetaItemOrLitParser<'a>>,
    pub(crate) span: Span,
}

impl<'a> MetaItemListParser<'a> {
    fn new(delim: &'a DelimArgs, dcx: DiagCtxtHandle<'a>) -> MetaItemListParser<'a> {
        MetaItemListParser::new_tts(delim.tokens.iter(), delim.dspan.entire(), dcx)
    }

    fn new_tts(tts: TokenStreamIter<'a>, span: Span, dcx: DiagCtxtHandle<'a>) -> Self {
        MetaItemListParserContext {
            inside_delimiters: tts.clone().peekable(),
            dcx,
        }
        .parse(span)
    }

    /// Lets you pick and choose as what you want to parse each element in the list
    pub(crate) fn mixed<'s>(&'s self) -> impl Iterator<Item = &'s MetaItemOrLitParser<'a>> + 's {
        self.sub_parsers.iter()
    }

    pub(crate) fn len(&self) -> usize {
        self.sub_parsers.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Asserts that every item in the list is another list starting with a word.
    ///
    /// See [`MetaItemParser::word`] for examples of words.
    pub(crate) fn all_word_list<'s>(&'s self) -> Option<Vec<(Ident, &'s ArgParser<'a>)>> {
        self.mixed().map(|i| i.meta_item()?.word()).collect()
    }

    /// Asserts that every item in the list is another list starting with a full path.
    ///
    /// See [`MetaItemParser::path`] for examples of paths.
    pub(crate) fn all_path_list<'s>(&'s self) -> Option<Vec<(PathParser<'a>, &'s ArgParser<'a>)>> {
        self.mixed().map(|i| Some(i.meta_item()?.path())).collect()
    }

    /// Returns Some if the list contains only a single element.
    ///
    /// Inside the Some is the parser to parse this single element.
    pub(crate) fn single(&self) -> Option<&MetaItemOrLitParser<'a>> {
        let mut iter = self.mixed();
        iter.next().filter(|_| iter.next().is_none())
    }
}
