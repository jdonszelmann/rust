use rustc_attr_data_structures::AttributeKind;
use rustc_span::{hygiene::Transparency, Span};
use rustc_span::sym;

use super::{AcceptContext, AttributeOrder, OnDuplicate, SingleAttributeParser};
use crate::parser::ArgParser;

pub(crate) struct TransparencyParser;

// FIXME(jdonszelmann): make these proper diagnostics
#[allow(rustc::untranslatable_diagnostic)]
#[allow(rustc::diagnostic_outside_of_impl)]
impl SingleAttributeParser for TransparencyParser {
    const PATH: &'static [rustc_span::Symbol] = &[sym::rustc_macro_transparency];
    const ATTRIBUTE_ORDER: AttributeOrder = AttributeOrder::KeepFirst;
    const ON_DUPLICATE: OnDuplicate = OnDuplicate::Custom(|cx, used, unused| {
        cx.dcx().span_err(vec![used, unused], "multiple macro transparency attributes");
    });

    fn convert(cx: &AcceptContext<'_>, args: &ArgParser<'_>) -> Option<AttributeKind> {
        match args.name_value().and_then(|nv| nv.value_as_str()) {
            Some(sym::transparent) => Some(Transparency::Transparent),
            Some(sym::semitransparent) => Some(Transparency::SemiTransparent),
            Some(sym::opaque) => Some(Transparency::Opaque),
            Some(other) => {
                cx.dcx().span_err(cx.attr_span, format!("unknown macro transparency: `{other}`"));
                None
            }
            None => None,
        }
        .map(AttributeKind::MacroTransparency)
    }
}
