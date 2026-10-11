//! An independently runnable snapshot of the Fun compiler.
pub mod ast;
pub mod core;
pub mod diagnostics;
pub mod elaborate;
pub mod eval;
pub mod layout;
pub mod lexer;
pub mod names;
pub mod parse;
pub mod session;
pub mod source;
pub mod typecheck;
pub mod types;

pub mod format;

pub mod fixity;

pub mod rename;
pub mod renamed;
pub mod scc;

pub mod core_lint;
pub mod core_type;

pub mod data;

pub mod desugar;
pub mod syn;

pub mod kinds;

pub mod classes;

pub mod primitives;

pub mod evidence;

pub mod io_lower;

pub mod match_compile;
pub mod usefulness;

pub mod derive;

pub mod anf;
pub mod erase;

pub mod anf_lint;
pub mod anf_machine;
pub mod anf_simplify;

pub mod env;
