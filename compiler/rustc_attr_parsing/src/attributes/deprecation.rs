use rustc_attr_data_structures::{AttributeKind, DeprecatedSince, Deprecation};
use rustc_feature::{AttributeTemplate, template};
use rustc_span::symbol::Ident;
use rustc_span::{Span, Symbol, sym};

use super::util::parse_version;
use super::{AttributeOrder, OnDuplicate, SingleAttributeParser};
use crate::context::{AcceptContext, Stage};
use crate::parser::ArgParser;
use crate::session_diagnostics;
use crate::session_diagnostics::UnsupportedLiteralReason;

pub(crate) struct DeprecationParser;

fn get<S: Stage>(
    cx: &AcceptContext<'_, '_, S>,
    ident: Ident,
    param_span: Span,
    arg: &ArgParser<'_>,
    item: &Option<Symbol>,
) -> Option<Symbol> {
    if item.is_some() {
        cx.duplicate_key(ident.span, ident.name);
        return None;
    }
    if let Some(v) = arg.name_value() {
        if let Some(value_str) = v.value_as_str() {
            Some(value_str)
        } else {
            let lit = v.value_as_lit();
            cx.emit_err(session_diagnostics::UnsupportedLiteral {
                span: v.value_span,
                reason: UnsupportedLiteralReason::DeprecatedString,
                is_bytestr: lit.kind.is_bytestr(),
                start_point_span: cx.sess().source_map().start_point(lit.span),
            });
            None
        }
    } else {
        cx.expected_name_value(param_span, Some(ident.name));
        None
    }
}

impl<S: Stage> SingleAttributeParser<S> for DeprecationParser {
    const PATH: &'static [rustc_span::Symbol] = &[sym::deprecated];
    const ATTRIBUTE_ORDER: AttributeOrder = AttributeOrder::KeepFirst;
    const ON_DUPLICATE: OnDuplicate<S> = OnDuplicate::Error;
    const TEMPLATE: AttributeTemplate = template!(
        Word,
        List: r#"/*opt*/ since = "version", /*opt*/ note = "reason""#,
        NameValueStr: "reason"
    );

    fn convert(cx: &AcceptContext<'_, '_, S>, args: &ArgParser<'_>) -> Option<AttributeKind> {
        let features = cx.features();

        let mut since = None;
        let mut note = None;
        let mut suggestion = None;

        let is_rustc = features.staged_api();

        if let Some(value) = args.name_value()
            && let Some(value_str) = value.value_as_str()
        {
            note = Some(value_str)
        } else if let Some(list) = args.list() {
            for param in list.mixed() {
                let param_span = param.span();
                let Some(param) = param.meta_item() else {
                    cx.emit_err(session_diagnostics::UnsupportedLiteral {
                        span: param_span,
                        reason: UnsupportedLiteralReason::DeprecatedKvPair,
                        is_bytestr: false,
                        start_point_span: cx.sess().source_map().start_point(param_span),
                    });
                    return None;
                };

                let (ident, arg) = param.word_or_empty();

                match ident.name {
                    sym::since => {
                        since = Some(get(cx, ident, param_span, arg, &since)?);
                    }
                    sym::note => {
                        note = Some(get(cx, ident, param_span, arg, &note)?);
                    }
                    sym::suggestion => {
                        if !features.deprecated_suggestion() {
                            cx.emit_err(session_diagnostics::DeprecatedItemSuggestion {
                                span: param_span,
                                is_nightly: cx.sess().is_nightly_build(),
                                details: (),
                            });
                        }

                        suggestion = Some(get(cx, ident, param_span, arg, &suggestion)?);
                    }
                    _ => {
                        cx.unknown_key(
                            param_span,
                            ident.to_string(),
                            if features.deprecated_suggestion() {
                                &["since", "note", "suggestion"]
                            } else {
                                &["since", "note"]
                            },
                        );
                        return None;
                    }
                }
            }
        }

        let since = if let Some(since) = since {
            if since.as_str() == "TBD" {
                DeprecatedSince::Future
            } else if !is_rustc {
                DeprecatedSince::NonStandard(since)
            } else if let Some(version) = parse_version(since) {
                DeprecatedSince::RustcVersion(version)
            } else {
                cx.emit_err(session_diagnostics::InvalidSince { span: cx.attr_span });
                DeprecatedSince::Err
            }
        } else if is_rustc {
            cx.emit_err(session_diagnostics::MissingSince { span: cx.attr_span });
            DeprecatedSince::Err
        } else {
            DeprecatedSince::Unspecified
        };

        if is_rustc && note.is_none() {
            cx.emit_err(session_diagnostics::MissingNote { span: cx.attr_span });
            return None;
        }

        Some(AttributeKind::Deprecation {
            deprecation: Deprecation { since, note, suggestion },
            span: cx.attr_span,
        })
    }
}
