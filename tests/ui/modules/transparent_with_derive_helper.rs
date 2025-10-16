//@ edition:2018
//@ proc-macro: add_transparent_helper.rs
extern crate add_transparent_helper;
use add_transparent_helper::AddTransparentHelper;

#[derive(AddTransparentHelper)]
struct S {
    #[transparent]
    // not an error since it's a helper!
    foo: u32,
}

#[transparent]
//~^ ERROR the `#[transparent]` attribute is an experimental feature
mod x {}

fn main() {}
