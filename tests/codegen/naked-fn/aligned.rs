//@ compile-flags: -C no-prepopulate-passes -Copt-level=0
//@ needs-asm-support
//@ only-x86_64

#![crate_type = "lib"]
#![feature(naked_functions, fn_align)]
use std::arch::asm;

// CHECK: Function Attrs: naked
// CHECK-NEXT: define{{.*}}void @naked_empty()
// CHECK: align 16
#[repr(align(16))]
#[no_mangle]
#[naked]
pub unsafe extern "C" fn naked_empty() {
    // CHECK-NEXT: {{.+}}:
    // CHECK-NEXT: call void asm
    // CHECK-NEXT: unreachable
    asm!("ret", options(noreturn));
}
