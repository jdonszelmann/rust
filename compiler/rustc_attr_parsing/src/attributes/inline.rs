use rustc_attr_data_structures::{AttributeKind, InlineAttr};
use rustc_errors::{struct_span_code_err, E0534, E0535};
use rustc_span::sym;

use crate::{attributes::SingleAttributeParser, parser::ArgParser};

use super::{AcceptContext, AttributeOrder, OnDuplicate};

pub(crate) struct InlineParser;

impl SingleAttributeParser for InlineParser {
    const PATH: &'static [rustc_span::Symbol] = &[sym::inline];
    const ATTRIBUTE_ORDER: AttributeOrder = AttributeOrder::KeepLast;
    const ON_DUPLICATE: OnDuplicate = OnDuplicate::FutureWarn;

    fn convert(cx: &AcceptContext<'_>, args: &ArgParser<'_>) -> Option<AttributeKind> {
        match args {
            ArgParser::NoArgs => Some(AttributeKind::Inline(InlineAttr::Hint)),
            ArgParser::List(list) => {
                let Some(l) = list.single()  else {
                    struct_span_code_err!(cx.dcx(), cx.attr_span, E0534, "expected one argument").emit();
                    return None;
                };

                match l.lit().and_then(|i| i.value_str()) {
                    Some(sym::always) => Some(AttributeKind::Inline(InlineAttr::Always)),
                    Some(sym::never) => Some(AttributeKind::Inline(InlineAttr::Never)),
                    _ => {
                        struct_span_code_err!(cx.dcx(), l.span(), E0535, "invalid argument")
                            .with_help("valid inline arguments are `always` and `never`")
                            .emit();
                        return None
                    }
                }
            },
            ArgParser::NameValue(_) => {
                todo!()
            },
        }
    }
}
