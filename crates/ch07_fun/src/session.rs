//! A compilation owns its sources, names, options, and accumulated diagnostics.
use crate::{
    diagnostics::{Diagnostic, Severity},
    names::{Interner, Supply},
    source::SourceMap,
};
/// A pass failed after recording one or more diagnostics in the session.
#[derive(Debug)]
pub struct Stop;
/// Driver options shared by every compiler pass.
#[derive(Default)]
pub struct Options {
    pub lint: bool,
    pub no_prelude: bool,
    pub dump: Option<String>,
}
// ANCHOR: session
#[derive(Default)]
/// No pass needs a global interner, source map, or diagnostic sink.
pub struct Session {
    pub sources: SourceMap,
    pub interner: Interner,
    pub supply: Supply,
    pub options: Options,
    pub diagnostics: Vec<Diagnostic>,
}
impl Session {
    pub fn emit(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }
    pub fn finish<T>(&self, value: T) -> Result<T, Stop> {
        if self
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
        {
            Err(Stop)
        } else {
            Ok(value)
        }
    }
}
// ANCHOR_END: session
