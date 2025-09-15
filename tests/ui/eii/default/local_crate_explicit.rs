//@ run-pass
//@ check-run-results
#![feature(eii)]

#[eii(eii1)]
pub fn decl1(x: u64) {
    //~^ WARN function `decl1` is never used
    println!("default {x}");
}

#[eii1]
pub fn decl2(x: u64) {
    println!("explicit {x}");
}

fn main() {
    decl1(4);
}
