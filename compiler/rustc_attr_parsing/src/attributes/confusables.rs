use rustc_attr_data_structures::AttributeKind;
use rustc_span::{Span, Symbol, sym};
use thin_vec::ThinVec;

use super::{AttributeGroup, AttributeMapping};
use crate::context::AttributeGroupContext;
use crate::session_diagnostics;

// TODO: turn into CombineGroup?
#[derive(Default)]
pub(crate) struct ConfusablesGroup {
    confusables: ThinVec<Symbol>,
    first_span: Option<Span>,
}

impl AttributeGroup for ConfusablesGroup {
    const ATTRIBUTES: AttributeMapping<Self> = &[(&[sym::rustc_confusables], |this, cx, args| {
        let Some(list) = args.list() else {
            // FIXME(jdonszelmann): error when not a list? Bring validation code here.
            //       NOTE: currently subsequent attributes are silently ignored using
            //       tcx.get_attr().
            return;
        };

        if list.is_empty() {
            cx.emit_err(session_diagnostics::EmptyConfusables { span: cx.attr_span });
        }

        for param in list.mixed() {
            let span = param.span();

            let Some(lit) = param.lit() else {
                cx.emit_err(session_diagnostics::IncorrectMetaItem {
                    span,
                    suggestion: Some(session_diagnostics::IncorrectMetaItemSuggestion {
                        lo: span.shrink_to_lo(),
                        hi: span.shrink_to_hi(),
                    }),
                });
                continue;
            };

            this.confusables.push(lit.symbol);
        }

        this.first_span.get_or_insert(cx.attr_span);
    })];

    fn finalize(self, _cx: &AttributeGroupContext<'_>) -> Option<AttributeKind> {
        if self.confusables.is_empty() {
            return None;
        }

        Some(AttributeKind::Confusables {
            symbols: self.confusables,
            first_span: self.first_span.unwrap(),
        })
    }
}
