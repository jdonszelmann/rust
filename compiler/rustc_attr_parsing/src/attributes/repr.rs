use rustc_abi::Align;
use rustc_ast::{IntTy, LitIntType, LitKind, MetaItemLit, UintTy};
use rustc_attr_data_structures::{AttributeKind, IntType, ReprAttr};
use rustc_span::symbol::Ident;
use rustc_span::{Span, Symbol, sym};

use super::{CombineAttributeGroup, ConvertFn};
use crate::context::AttributeAcceptContext;
use crate::parser::{ArgParser, MetaItemListParser, MetaItemOrLitParser, MetaItemParser};
use crate::session_diagnostics;
use crate::session_diagnostics::IncorrectReprFormatGenericCause;

/// Parse #[repr(...)] forms.
///
/// Valid repr contents: any of the primitive integral type names (see
/// `int_type_of_word`, below) to specify enum discriminant type; `C`, to use
/// the same discriminant size that the corresponding C enum would or C
/// structure layout, `packed` to remove padding, and `transparent` to delegate representation
/// concerns to the only non-ZST field.
// FIXME(jdonszelmann): is a vec the right representation here even? isn't it just a struct?
pub(crate) struct ReprGroup;

impl CombineAttributeGroup for ReprGroup {
    type Item = (ReprAttr, Span);
    const PATH: &'static [rustc_span::Symbol] = &[sym::repr];
    const CONVERT: ConvertFn<Self::Item> = AttributeKind::Repr;

    fn extend<'a>(
        cx: &'a AttributeAcceptContext<'a>,
        args: &'a ArgParser<'a>,
    ) -> impl IntoIterator<Item = Self::Item> + 'a {
        let mut reprs = Vec::new();

        let Some(list) = args.list() else {
            return reprs;
        };

        for param in list.mixed() {
            let span = param.span();
            reprs.extend(param.meta_item().and_then(|mi| parse_repr(cx, &mi)).map(|r| (r, span)));
        }

        reprs
    }
}

fn parse_repr<'a, 'b>(
    cx: &AttributeAcceptContext<'_>,
    param: &'a MetaItemParser<'b>,
) -> Option<ReprAttr> {
    // FIXME(jdonszelmann): invert the parsing here to match on the word first and then the
    // structure.
    let (ident, args) = param.word_or_empty();

    if args.no_args() {
        parse_simple_repr(cx, ident, param.span())
    } else if let Some(list) = args.list() {
        parse_list_repr(cx, ident, list, param.span())
    } else if let Some(name_value) = args.name_value() {
        reject_name_value_repr(cx, ident, name_value.value_as_lit(), param.span());
        None
    } else {
        completely_unknown(cx, param.span());
        None
    }
}

fn parse_list_repr(
    cx: &AttributeAcceptContext<'_>,
    ident: Ident,
    list: &MetaItemListParser<'_>,
    param_span: Span,
) -> Option<ReprAttr> {
    if let Some(single) = list.single() {
        match single {
            MetaItemOrLitParser::MetaItemParser(meta) => {
                reject_not_literal_list(cx, ident, meta.span(), param_span);
                None
            }
            MetaItemOrLitParser::Lit(lit) => parse_singleton_list_repr(cx, ident, lit, param_span),
        }
    } else {
        reject_not_one_element_list(cx, ident, param_span);
        None
    }
}

fn reject_not_one_element_list(cx: &AttributeAcceptContext<'_>, ident: Ident, param_span: Span) {
    match ident.name {
        sym::align => {
            cx.dcx()
                .emit_err(session_diagnostics::IncorrectReprFormatAlignOneArg { span: param_span });
        }
        sym::packed => {
            cx.dcx().emit_err(session_diagnostics::IncorrectReprFormatPackedOneOrZeroArg {
                span: param_span,
            });
        }
        sym::Rust | sym::C | sym::simd | sym::transparent => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoParen {
                span: param_span,
                name: ident.to_string(),
            });
        }
        other if int_type_of_word(other).is_some() => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoParen {
                span: param_span,
                name: ident.to_string(),
            });
        }
        _ => {
            completely_unknown(cx, param_span);
        }
    }
}

fn reject_not_literal_list(
    cx: &AttributeAcceptContext<'_>,
    ident: Ident,
    meta_span: Span,
    param_span: Span,
) {
    match ident.name {
        sym::align => {
            cx.dcx().emit_err(session_diagnostics::IncorrectReprFormatExpectInteger {
                span: meta_span,
            });
        }

        sym::packed => {
            cx.dcx().emit_err(session_diagnostics::IncorrectReprFormatPackedExpectInteger {
                span: meta_span,
            });
        }
        sym::Rust | sym::C | sym::simd | sym::transparent => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoParen {
                span: param_span,
                name: ident.to_string(),
            });
        }
        other if int_type_of_word(other).is_some() => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoParen {
                span: param_span,
                name: ident.to_string(),
            });
        }
        _ => {
            completely_unknown(cx, param_span);
        }
    }
}

fn reject_name_value_repr(
    cx: &AttributeAcceptContext<'_>,
    ident: Ident,
    value: &MetaItemLit,
    param_span: Span,
) {
    match ident.name {
        sym::align | sym::packed => {
            cx.dcx().emit_err(session_diagnostics::IncorrectReprFormatGeneric {
                span: param_span,
                // FIXME(jdonszelmann) can just be a string in the diag type
                repr_arg: &ident.to_string(),
                cause: IncorrectReprFormatGenericCause::from_lit_kind(
                    param_span,
                    &value.kind,
                    ident.name.as_str(),
                ),
            });
        }
        sym::Rust | sym::C | sym::simd | sym::transparent => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoValue {
                span: param_span,
                name: ident.to_string(),
            });
        }
        other if int_type_of_word(other).is_some() => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoValue {
                span: param_span,
                name: ident.to_string(),
            });
        }
        _ => {
            completely_unknown(cx, param_span);
        }
    }
}

fn parse_singleton_list_repr(
    cx: &AttributeAcceptContext<'_>,
    ident: Ident,
    lit: &MetaItemLit,
    param_span: Span,
) -> Option<ReprAttr> {
    match ident.name {
        sym::align => match parse_alignment(&lit.kind) {
            Ok(literal) => Some(ReprAttr::ReprAlign(literal)),
            Err(message) => {
                cx.dcx().emit_err(session_diagnostics::InvalidReprGeneric {
                    span: lit.span,
                    repr_arg: ident.to_string(),
                    error_part: message,
                });
                None
            }
        },
        sym::packed => match parse_alignment(&lit.kind) {
            Ok(literal) => Some(ReprAttr::ReprPacked(literal)),
            Err(message) => {
                cx.dcx().emit_err(session_diagnostics::InvalidReprGeneric {
                    span: lit.span,
                    repr_arg: ident.to_string(),
                    error_part: message,
                });
                None
            }
        },
        sym::Rust | sym::C | sym::simd | sym::transparent => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoParen {
                span: param_span,
                name: ident.to_string(),
            });
            None
        }
        other if int_type_of_word(other).is_some() => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprHintNoParen {
                span: param_span,
                name: ident.to_string(),
            });
            None
        }
        _ => {
            completely_unknown(cx, param_span);
            None
        }
    }
}

fn parse_simple_repr(
    cx: &AttributeAcceptContext<'_>,
    ident: Ident,
    param_span: Span,
) -> Option<ReprAttr> {
    use ReprAttr::*;
    match ident.name {
        sym::Rust => Some(ReprRust),
        sym::C => Some(ReprC),
        sym::packed => Some(ReprPacked(Align::ONE)),
        sym::simd => Some(ReprSimd),
        sym::transparent => Some(ReprTransparent),
        sym::align => {
            cx.dcx().emit_err(session_diagnostics::InvalidReprAlignNeedArg { span: ident.span });
            None
        }
        other => {
            if let Some(int) = int_type_of_word(other) {
                Some(ReprInt(int))
            } else {
                completely_unknown(cx, param_span);
                None
            }
        }
    }
}

fn completely_unknown(cx: &AttributeAcceptContext<'_>, param_span: Span) {
    cx.dcx().emit_err(session_diagnostics::UnrecognizedReprHint { span: param_span });
}

fn int_type_of_word(s: Symbol) -> Option<IntType> {
    use IntType::*;

    match s {
        sym::i8 => Some(SignedInt(IntTy::I8)),
        sym::u8 => Some(UnsignedInt(UintTy::U8)),
        sym::i16 => Some(SignedInt(IntTy::I16)),
        sym::u16 => Some(UnsignedInt(UintTy::U16)),
        sym::i32 => Some(SignedInt(IntTy::I32)),
        sym::u32 => Some(UnsignedInt(UintTy::U32)),
        sym::i64 => Some(SignedInt(IntTy::I64)),
        sym::u64 => Some(UnsignedInt(UintTy::U64)),
        sym::i128 => Some(SignedInt(IntTy::I128)),
        sym::u128 => Some(UnsignedInt(UintTy::U128)),
        sym::isize => Some(SignedInt(IntTy::Isize)),
        sym::usize => Some(UnsignedInt(UintTy::Usize)),
        _ => None,
    }
}

pub(crate) fn parse_alignment(node: &LitKind) -> Result<Align, &'static str> {
    if let LitKind::Int(literal, LitIntType::Unsuffixed) = node {
        // `Align::from_bytes` accepts 0 as an input, check is_power_of_two() first
        if literal.get().is_power_of_two() {
            // Only possible error is larger than 2^29
            literal
                .get()
                .try_into()
                .ok()
                .and_then(|v| Align::from_bytes(v).ok())
                .ok_or("larger than 2^29")
        } else {
            Err("not a power of two")
        }
    } else {
        Err("not an unsuffixed integer")
    }
}
