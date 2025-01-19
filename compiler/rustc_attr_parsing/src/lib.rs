//! Centralized logic for parsing and attributes.
//!
//! Part of a series of crates:
//! - rustc_attr_data_structures: contains types that the parsers parse into
//! - rustc_attr_parsing: this crate
//! - (in the future): rustc_attr_validation
//!
//! History: Check out [#131229](https://github.com/rust-lang/rust/issues/131229).
//! There used to be only one definition of attributes in the compiler: `ast::Attribute`.
//! These were then parsed or validated or both in places distributed all over the compiler.
//! This was a mess...
//!
//! Attributes are markers on items. Most are actually attribute-like proc-macros, and are expanded
//! but some remain as the so-called built-in attributes. These are not macros at all, and really
//! are just markers to guide the compilation process. An example is `#[inline(...)]` which changes
//! how code for functions is generated. Built-in attributes aren't macros because there's no rust
//! syntax they could expand to.
//!
//! In this crate, syntactical attributes (sequences of tokens that look like
//! `#[something(something else)]`) are parsed into more semantic attributes, markers on items.
//! Multiple syntactic attributes might influence a single semantic attribute. For example,
//! `#[stable(...)]` and `#[unstable()]` cannot occur together, and both semantically define
//! a "stability" of an item. So, the stability attribute has an
//! [`AttributeParser`](attributes::AttributeParser) that recognizes both the `#[stable()]`
//! and `#[unstable()]` syntactic attributes, and at the end produce a single [`AttributeKind::Stability`].
//!
//! As a rule of thumb, when a syntactical attribute can be applied more than once, they should be
//! combined into a single semantic attribute. For example:
//!
//! ```
//! #[repr(C)]
//! #[repr(packed)]
//! struct Meow {}
//! ```
//!
//! should result in a single `AttributeKind::Repr` containing a list of repr annotations, in this
//! case `C` and `packed`. This is equivalent to writing `#[repr(C, packed)]` in a single
//! syntactical annotation.

// tidy-alphabetical-start
#![allow(internal_features)]
#![doc(rust_logo)]
#![feature(let_chains)]
#![feature(rustdoc_internals)]
#![warn(unreachable_pub)]
// tidy-alphabetical-end

#[macro_use]
mod attributes;
mod context;
pub mod parser;
mod session_diagnostics;

pub use attributes::cfg::*;
pub use attributes::util::{find_crate_name, is_builtin_attr, parse_version};
pub use context::{AttributeParser, OmitDoc};
pub use rustc_attr_data_structures::*;

rustc_fluent_macro::fluent_messages! { "../messages.ftl" }

/// Finds attributes in sequences of attributes by pattern matching.
///
/// A little like `matches` but for attributes.
///
/// ```rust,no_run
/// // finds the repr attribute
/// if let Some(r) = find_attr!(attrs, AttributeKind::Repr(r) => r) {
///
/// }
///
/// // checks if one has matched
/// if find_attr!(attrs, AttributeKind::Repr(_)) {
///
/// }
/// ```
#[macro_export]
macro_rules! find_attr {
    ($attributes_list: expr, $pattern: pat $(if $guard: expr)?) => {{
        $crate::find_attr!($attributes_list, $pattern $(if $guard)? => ()).is_some()
    }};

    ($attributes_list: expr, $pattern: pat $(if $guard: expr)? => $e: expr) => {{
        fn check_attribute_iterator<'a>(_: &'_ impl IntoIterator<Item = &'a rustc_hir::Attribute>) {}
        check_attribute_iterator(&$attributes_list);

        let find_attribute = |iter| {
            for i in $attributes_list {
                match i {
                    rustc_hir::Attribute::Parsed($pattern) $(if $guard: expr)? => {
                        return Some($e);
                    }
                    _ => {}
                }
            }

            None
        };
        find_attribute($attributes_list)
    }};
}
