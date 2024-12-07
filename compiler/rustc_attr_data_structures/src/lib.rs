// tidy-alphabetical-start
#![allow(internal_features)]
#![doc(rust_logo)]
#![feature(let_chains)]
#![feature(rustdoc_internals)]
#![warn(unreachable_pub)]
// tidy-alphabetical-end

mod attributes;
mod stability;
mod version;

pub use attributes::*;
pub use version::*;
pub use stability::*;

/// Requirements for a `StableHashingContext` to be used in this crate.
/// This is a hack to allow using the `HashStable_Generic` derive macro
/// instead of implementing everything in `rustc_middle`.
pub trait HashStableContext:
    rustc_ast::HashStableContext + rustc_abi::HashStableContext
{}


// pub trait HashStableContext: rustc_span::HashStableContext {}

