use rustc_ast::{ast, tokenstream::TokenStream};
use rustc_expand::base::{Annotatable, ExtCtxt, MacroExpanderResult};
use rustc_span::{sym, Span};

use crate::util::{check_builtin_macro_attribute, warn_on_duplicate_attribute};

pub(crate) fn expand_define_global_registry(
    cx: &mut ExtCtxt<'_>,
    _attr_sp: Span,
    meta_item: &ast::MetaItem,
    item: Annotatable,
) -> Vec<Annotatable> {
    check_builtin_macro_attribute(cx, meta_item, sym::global_registry);
    warn_on_duplicate_attribute(cx, &item, sym::global_registry);

    todo!()
}

pub(crate) fn expand_register<'cx>(
    _ecx: &'cx mut ExtCtxt<'_>,
    _sp: Span,
    _tts: TokenStream,
) -> MacroExpanderResult<'cx> {
    todo!()
}
