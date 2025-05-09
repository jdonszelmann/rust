use rustc_ast::ptr::P;
use rustc_ast::token::{Delimiter, TokenKind};
use rustc_ast::tokenstream::{DelimSpacing, DelimSpan, Spacing, TokenStream, TokenTree};
use rustc_ast::{DUMMY_NODE_ID, EIIImpl, EIIMacroFor, ItemKind, ast, token, tokenstream};
use rustc_ast_pretty::pprust::path_to_string;
use rustc_expand::base::{Annotatable, ExtCtxt};
use rustc_span::{Ident, Span, kw, sym};

// ```rust
// #[eii]
// fn panic_handler();
//
// // or:
//
// #[eii(panic_handler)]
// fn panic_handler();
//
// // expansion:
//
// extern "Rust" {
//     fn panic_handler();
// }
//
// #[rustc_builtin_macro(eii_macro)]
// #[eii_macro_for(panic_handler)]
// macro panic_handler() {}
// ```
pub(crate) fn eii(
    ecx: &mut ExtCtxt<'_>,
    span: Span,
    meta_item: &ast::MetaItem,
    item: Annotatable,
) -> Vec<Annotatable> {
    eii_(ecx, span, meta_item, item, false)
}

pub(crate) fn unsafe_eii(
    ecx: &mut ExtCtxt<'_>,
    span: Span,
    meta_item: &ast::MetaItem,
    item: Annotatable,
) -> Vec<Annotatable> {
    eii_(ecx, span, meta_item, item, true)
}

fn eii_(
    ecx: &mut ExtCtxt<'_>,
    span: Span,
    meta_item: &ast::MetaItem,
    item: Annotatable,
    impl_unsafe: bool,
) -> Vec<Annotatable> {
    let span = ecx.with_def_site_ctxt(span);

    let Annotatable::Item(item) = item else {
        ecx.dcx()
            .emit_err(EIIMacroExpectedFunction { span, name: path_to_string(&meta_item.path) });
        return vec![item];
    };

    let orig_item = item.clone();

    let item = item.into_inner();

    let ast::Item { attrs, id: _, span: item_span, vis, kind: ItemKind::Fn(mut func), tokens: _ } =
        item
    else {
        ecx.dcx()
            .emit_err(EIIMacroExpectedFunction { span, name: path_to_string(&meta_item.path) });
        return vec![Annotatable::Item(P(item))];
    };

    let macro_name = if meta_item.is_word() {
        func.ident
    } else if let Some([first]) = meta_item.meta_item_list()
        && let Some(m) = first.meta_item()
        && m.path.segments.len() == 1
    {
        m.path.segments[0].ident
    } else {
        ecx.dcx().emit_err(EIIMacroExpectedMaxOneArgument {
            span: meta_item.span,
            name: path_to_string(&meta_item.path),
        });
        return vec![Annotatable::Item(orig_item)];
    };

    let abi = match func.sig.header.ext {
        // extern "X" fn  =>  extern "X" {}
        ast::Extern::Explicit(lit, _) => Some(lit),
        // extern fn  =>  extern {}
        ast::Extern::Implicit(_) => None,
        // fn  =>  extern "Rust" {}
        ast::Extern::None => Some(ast::StrLit {
            symbol: sym::Rust,
            suffix: None,
            symbol_unescaped: sym::Rust,
            style: ast::StrStyle::Cooked,
            span,
        }),
    };

    // ABI has been moved to the extern {} block, so we remove it from the fn item.
    func.sig.header.ext = ast::Extern::None;

    // And mark safe functions explicitly as `safe fn`.
    if func.sig.header.safety == ast::Safety::Default {
        func.sig.header.safety = ast::Safety::Safe(func.sig.span);
    }

    // extern "…" { safe fn item(); }
    let extern_block = Annotatable::Item(P(ast::Item {
        attrs: ast::AttrVec::default(),
        id: ast::DUMMY_NODE_ID,
        span,
        vis: ast::Visibility { span, kind: ast::VisibilityKind::Inherited, tokens: None },
        kind: ast::ItemKind::ForeignMod(ast::ForeignMod {
            extern_span: span,
            safety: ast::Safety::Unsafe(span),
            abi,
            items: From::from([P(ast::ForeignItem {
                attrs,
                id: ast::DUMMY_NODE_ID,
                span: item_span,
                vis,
                kind: ast::ForeignItemKind::Fn(func.clone()),
                tokens: None,
            })]),
        }),
        tokens: None,
    }));

    let macro_def = Annotatable::Item(P(ast::Item {
        attrs: ast::AttrVec::from_iter([
            // #[builtin_macro(eii_macro)]
            ast::Attribute {
                kind: ast::AttrKind::Normal(P(ast::NormalAttr {
                    item: ast::AttrItem {
                        unsafety: ast::Safety::Default,
                        path: ast::Path::from_ident(Ident::new(sym::rustc_builtin_macro, span)),
                        args: ast::AttrArgs::Delimited(ast::DelimArgs {
                            dspan: DelimSpan::from_single(span),
                            delim: Delimiter::Parenthesis,
                            tokens: TokenStream::new(vec![tokenstream::TokenTree::token_alone(
                                token::TokenKind::Ident(sym::eii_macro, token::IdentIsRaw::No),
                                span,
                            )]),
                        }),
                        tokens: None,
                    },
                    tokens: None,
                })),
                id: ecx.sess.psess.attr_id_generator.mk_attr_id(),
                style: ast::AttrStyle::Outer,
                span,
            },
        ]),
        id: ast::DUMMY_NODE_ID,
        span,
        // pub
        vis: ast::Visibility { span, kind: ast::VisibilityKind::Public, tokens: None },
        kind: ast::ItemKind::MacroDef(
            // macro macro_name
            macro_name,
            ast::MacroDef {
                // { () => {} }
                body: P(ast::DelimArgs {
                    dspan: DelimSpan::from_single(span),
                    delim: Delimiter::Brace,
                    tokens: TokenStream::from_iter([
                        TokenTree::Delimited(
                            DelimSpan::from_single(span),
                            DelimSpacing::new(Spacing::Alone, Spacing::Alone),
                            Delimiter::Parenthesis,
                            TokenStream::default(),
                        ),
                        TokenTree::token_alone(TokenKind::FatArrow, span),
                        TokenTree::Delimited(
                            DelimSpan::from_single(span),
                            DelimSpacing::new(Spacing::Alone, Spacing::Alone),
                            Delimiter::Brace,
                            TokenStream::default(),
                        ),
                    ]),
                }),
                macro_rules: false,
                // #[eii_macro_for(func.ident)]
                eii_macro_for: Some(ast::EIIMacroFor {
                    extern_item_path: ast::Path::from_ident(func.ident),
                    impl_unsafe,
                }),
            },
        ),
        tokens: None,
    }));

    vec![extern_block, macro_def]
}

use crate::errors::{
    EIIMacroExpectedFunction, EIIMacroExpectedMaxOneArgument, EIIMacroForExpectedList,
    EIIMacroForExpectedMacro, EIIMacroForExpectedUnsafe,
};

pub(crate) fn eii_macro_for(
    ecx: &mut ExtCtxt<'_>,
    span: Span,
    meta_item: &ast::MetaItem,
    mut item: Annotatable,
) -> Vec<Annotatable> {
    let Annotatable::Item(i) = &mut item else {
        ecx.dcx().emit_err(EIIMacroForExpectedMacro { span });
        return vec![item];
    };
    let ItemKind::MacroDef(_, d) = &mut i.kind else {
        ecx.dcx().emit_err(EIIMacroForExpectedMacro { span });
        return vec![item];
    };

    let Some(list) = meta_item.meta_item_list() else {
        ecx.dcx().emit_err(EIIMacroForExpectedList { span: meta_item.span });
        return vec![item];
    };

    if list.len() > 2 {
        ecx.dcx().emit_err(EIIMacroForExpectedList { span: meta_item.span });
        return vec![item];
    }

    let Some(extern_item_path) = list.get(0).and_then(|i| i.meta_item()).map(|i| i.path.clone())
    else {
        ecx.dcx().emit_err(EIIMacroForExpectedList { span: meta_item.span });
        return vec![item];
    };

    let impl_unsafe = if let Some(i) = list.get(1) {
        if i.lit().and_then(|i| i.kind.str()).is_some_and(|i| i == kw::Unsafe) {
            true
        } else {
            ecx.dcx().emit_err(EIIMacroForExpectedUnsafe { span: i.span() });
            return vec![item];
        }
    } else {
        false
    };

    d.eii_macro_for = Some(EIIMacroFor { extern_item_path, impl_unsafe });

    // Return the original item and the new methods.
    vec![item]
}

pub(crate) fn eii_macro(
    ecx: &mut ExtCtxt<'_>,
    span: Span,
    meta_item: &ast::MetaItem,
    mut item: Annotatable,
) -> Vec<Annotatable> {
    let Annotatable::Item(i) = &mut item else {
        ecx.dcx()
            .emit_err(EIIMacroExpectedFunction { span, name: path_to_string(&meta_item.path) });
        return vec![item];
    };

    let ItemKind::Fn(f) = &mut i.kind else {
        ecx.dcx()
            .emit_err(EIIMacroExpectedFunction { span, name: path_to_string(&meta_item.path) });
        return vec![item];
    };

    let is_default = if meta_item.is_word() {
        false
    } else if let Some([first]) = meta_item.meta_item_list()
        && let Some(m) = first.meta_item()
        && m.path.segments.len() == 1
    {
        m.path.segments[0].ident.name == kw::Default
    } else {
        ecx.dcx().emit_err(EIIMacroExpectedMaxOneArgument {
            span: meta_item.span,
            name: path_to_string(&meta_item.path),
        });
        return vec![item];
    };

    f.eii_impl.push(EIIImpl {
        node_id: DUMMY_NODE_ID,
        eii_macro_path: meta_item.path.clone(),
        impl_safety: meta_item.unsafety,
        span,
        inner_span: meta_item.path.span,
        is_default,
    });

    vec![item]
}
