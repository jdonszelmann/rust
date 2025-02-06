use rustc_span::{sym, Span};

use crate::attributes::SingleAttributeParser;

pub(crate) struct InlineParser;

impl SingleAttributeParser for InlineParser {
    const PATH: &'static [rustc_span::Symbol] = &[sym::inline];

    fn on_duplicate(cx: &super::AcceptContext<'_>, used: Span, unused: Span) {
        todo!()
    }

    fn convert(cx: &super::AcceptContext<'_>, args: &crate::parser::ArgParser<'_>) -> Option<rustc_attr_data_structures::AttributeKind> {
        todo!()
    }
}
