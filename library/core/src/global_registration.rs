//! TODO: docs
#![stable(feature = "global_registration", since = "CURRENT_RUSTC_VERSION")]
#![cfg(not(bootstrap))]

/// TODO docs
#[stable(feature = "global_registration", since = "CURRENT_RUSTC_VERSION")]
#[rustc_builtin_macro]
pub macro register($name: ident, $($value: expr),* $(,)?) {
    /* compiler built-in */
}

/// TODO: docs
#[stable(feature = "global_registration", since = "CURRENT_RUSTC_VERSION")]
#[rustc_builtin_macro]
pub macro global_registry($item:item) {
    /* compiler built-in */
}
