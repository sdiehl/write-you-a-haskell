//! The first independently runnable Fun compiler snapshot.
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
