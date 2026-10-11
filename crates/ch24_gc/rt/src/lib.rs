//! Native runtime ABI and the shared, safe primitive semantics used by interpreters.
#![deny(unsafe_op_in_unsafe_fn)]
#[cfg(not(all(
    target_pointer_width = "64",
    any(target_arch = "x86_64", target_arch = "aarch64"),
    any(target_os = "linux", target_os = "macos")
)))]
compile_error!("Fun supports 64-bit Linux and macOS on x86-64 and AArch64");
pub mod abi;
pub mod apply;
pub mod copying;
pub mod heap;
pub mod layout;
mod primitives;
pub use abi::{rt_div_zero, rt_error, rt_exit, rt_pattern_fail};
pub use apply::{
    rt_apply, rt_apply1, rt_apply2, rt_apply3, rt_apply4, rt_closure_set,
    rt_make_pap,
};
pub use heap::{rt_alloc, rt_alloc_atomic, rt_init};
pub use primitives::{
    ALL, Failure, Prim, Value, evaluate, quote, read_int, show_double,
    trace,
};
