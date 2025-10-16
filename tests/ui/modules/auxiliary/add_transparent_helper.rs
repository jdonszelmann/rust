extern crate proc_macro;
use proc_macro::*;

#[proc_macro_derive(AddTransparentHelper, attributes(transparent))]
pub fn add_transparent_helper(_ts: TokenStream) -> TokenStream {
    TokenStream::new()
}
