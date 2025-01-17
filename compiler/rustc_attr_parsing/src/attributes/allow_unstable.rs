use std::iter;

use rustc_attr_data_structures::AttributeKind;
use rustc_span::{sym, Span, Symbol};

use super::{CombineAttributeGroup, ConvertFn};
use crate::context::AttributeAcceptContext;
use crate::parser::ArgParser;
use crate::session_diagnostics;

pub(crate) struct AllowInternalUnstableGroup;
impl CombineAttributeGroup for AllowInternalUnstableGroup {
    const PATH: &'static [rustc_span::Symbol] = &[sym::allow_internal_unstable];
    type Item = (Symbol, Span);
    const CONVERT: ConvertFn<Self::Item> = AttributeKind::AllowInternalUnstable;

    fn extend<'a>(
        cx: &'a AttributeAcceptContext<'a>,
        args: &'a ArgParser<'a>,
    ) -> impl IntoIterator<Item = Self::Item> + 'a {
        parse_unstable(cx, args, Self::PATH[0]).into_iter().zip(iter::repeat(cx.attr_span))
    }
}

pub(crate) struct AllowConstFnUnstableGroup;
impl CombineAttributeGroup for AllowConstFnUnstableGroup {
    const PATH: &'static [rustc_span::Symbol] = &[sym::rustc_allow_const_fn_unstable];
    type Item = Symbol;
    const CONVERT: ConvertFn<Self::Item> = AttributeKind::AllowConstFnUnstable;

    fn extend<'a>(
        cx: &'a AttributeAcceptContext<'a>,
        args: &'a ArgParser<'a>,
    ) -> impl IntoIterator<Item = Self::Item> + 'a {
        parse_unstable(cx, args, Self::PATH[0])
    }
}

fn parse_unstable<'a>(
    cx: &AttributeAcceptContext<'_>,
    args: &'a ArgParser<'a>,
    symbol: Symbol,
) -> impl IntoIterator<Item = Symbol> {
    let mut res = Vec::new();

    let Some(list) = args.list() else {
        cx.emit_err(session_diagnostics::ExpectsFeatureList {
            span: cx.attr_span,
            name: symbol.to_ident_string(),
        });
        return res;
    };

    for param in list.mixed() {
        let param_span = param.span();
        if let Some(ident) = param.meta_item().and_then(|i| i.word_without_args()) {
            res.push(ident.name);
        } else {
            cx.emit_err(session_diagnostics::ExpectsFeatures {
                span: param_span,
                name: symbol.to_ident_string(),
            });
        }
    }

    res
}
