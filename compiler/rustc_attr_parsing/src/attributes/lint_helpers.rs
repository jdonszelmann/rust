use rustc_attr_data_structures::{AttributeKind, LintLevelItem, LintLevels};
use rustc_feature::{AttributeTemplate, template};
use rustc_span::{Span, Symbol, sym};

use super::{AcceptMapping, AttributeParser, NoArgsAttributeParser};
use crate::attributes::OnDuplicate;
use crate::context::{AcceptContext, FinalizeContext, Stage};
use crate::parser::{ArgParser, MetaItemOrLitParser};

pub(crate) struct AsPtrParser;
impl<S: Stage> NoArgsAttributeParser<S> for AsPtrParser {
    const PATH: &[Symbol] = &[sym::rustc_as_ptr];
    const ON_DUPLICATE: OnDuplicate<S> = OnDuplicate::Error;
    const CREATE: fn(Span) -> AttributeKind = AttributeKind::AsPtr;
}

pub(crate) struct PubTransparentParser;
impl<S: Stage> NoArgsAttributeParser<S> for PubTransparentParser {
    const PATH: &[Symbol] = &[sym::rustc_pub_transparent];
    const ON_DUPLICATE: OnDuplicate<S> = OnDuplicate::Error;
    const CREATE: fn(Span) -> AttributeKind = AttributeKind::PubTransparent;
}

#[derive(Default)]
pub(crate) struct LintLevelParser {
    lint_levels: LintLevels,
}

impl LintLevelParser {
    const TEMPLATE: AttributeTemplate =
        template!(List: r#"lint1, lint2, ..., /*opt*/ reason = "...""#);

    fn accept<'cx, S: Stage>(
        &mut self,
        cx: &'cx AcceptContext<'_, '_, S>,
        args: &ArgParser<'_>,
        sym: Symbol,
    ) {
        let Some(list) = args.list() else {
            cx.expected_list(args.span().unwrap_or(cx.attr_path.span));
            return;
        };

        let mut reason = None;
        let mut lints = Vec::new();

        for lint_or_reason in list.mixed() {
            let lint_or_reason = match lint_or_reason {
                MetaItemOrLitParser::MetaItemParser(mip) => mip,
                MetaItemOrLitParser::Lit(lit) => {
                    cx.unexpected_literal(lit.span);
                    continue;
                }
                MetaItemOrLitParser::Err(..) => {
                    // error already emitted
                    continue;
                }
            };

            let path = lint_or_reason.path();
            let args = lint_or_reason.args();

            // TODO: reason must come last
            if path.word_is(sym::reason) {
                let Some(nv) = args.name_value() else {
                    cx.expected_name_value(args.span().unwrap_or(path.span()), Some(sym::reason));
                    continue;
                };

                let Some(lit_str) = nv.value_as_str() else {
                    cx.expected_string_literal(nv.value_span, Some(nv.value_as_lit()));
                    continue;
                };

                if reason.is_some() {
                    cx.duplicate_key(path.span(), sym::reason);
                    continue;
                }

                reason = Some(lit_str)
            } else {
                if let Err(span) = args.no_args() {
                    cx.expected_no_args(span);
                }

                lints.push((path.get_attribute_path(), path.span()))
            }
        }

        let list = match sym {
            sym::allow => &mut self.lint_levels.allowed,
            sym::expect => &mut self.lint_levels.expected,
            sym::warn => &mut self.lint_levels.warned,
            sym::deny => &mut self.lint_levels.denied,
            sym::forbid => &mut self.lint_levels.forbidden,
            _ => unreachable!(),
        };

        for (lint_name, span) in lints {
            list.push(LintLevelItem { lint_name, reason, span });
        }
    }
}

impl<S: Stage> AttributeParser<S> for LintLevelParser {
    const ATTRIBUTES: AcceptMapping<Self, S> = &[
        (&[sym::allow], Self::TEMPLATE, |this, cx, args| this.accept(cx, args, sym::allow)),
        (&[sym::expect], Self::TEMPLATE, |this, cx, args| this.accept(cx, args, sym::expect)),
        (&[sym::warn], Self::TEMPLATE, |this, cx, args| this.accept(cx, args, sym::warn)),
        (&[sym::deny], Self::TEMPLATE, |this, cx, args| this.accept(cx, args, sym::deny)),
        (&[sym::forbid], Self::TEMPLATE, |this, cx, args| this.accept(cx, args, sym::forbid)),
    ];

    fn finalize(self, _cx: &FinalizeContext<'_, '_, S>) -> Option<AttributeKind> {
        let LintLevels { allowed, expected, warned, denied, forbidden } = &self.lint_levels;

        if allowed.is_empty()
            && expected.is_empty()
            && warned.is_empty()
            && denied.is_empty()
            && forbidden.is_empty()
        {
            return None;
        }

        Some(AttributeKind::LintLevels(self.lint_levels))
    }
}

pub(crate) struct PassByValueParser;
impl<S: Stage> NoArgsAttributeParser<S> for PassByValueParser {
    const PATH: &[Symbol] = &[sym::rustc_pass_by_value];
    const ON_DUPLICATE: OnDuplicate<S> = OnDuplicate::Error;
    const CREATE: fn(Span) -> AttributeKind = AttributeKind::PassByValue;
}

pub(crate) struct AutomaticallyDerivedParser;
impl<S: Stage> NoArgsAttributeParser<S> for AutomaticallyDerivedParser {
    const PATH: &[Symbol] = &[sym::automatically_derived];
    const ON_DUPLICATE: OnDuplicate<S> = OnDuplicate::Warn;
    const CREATE: fn(Span) -> AttributeKind = AttributeKind::AutomaticallyDerived;
}
