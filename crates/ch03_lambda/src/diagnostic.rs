//! Stable diagnostic headers followed by an ariadne source excerpt.
use ariadne::{Config, IndexType, Label, Report, ReportKind, Source};
use std::{
    io::{self, Write},
    ops::Range,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub code: &'static str,
    pub message: String,
    pub span: Range<usize>,
}

impl Diagnostic {
    pub fn new(
        code: &'static str,
        message: impl Into<String>,
        span: Range<usize>,
    ) -> Self {
        Self {
            code,
            message: message.into(),
            span,
        }
    }
    /// Render byte spans correctly even when preceding text contains Unicode.
    pub fn render(
        &self,
        file: &str,
        source: &str,
        color: bool,
        mut out: impl Write,
    ) -> io::Result<()> {
        let prefix = &source[..self.span.start.min(source.len())];
        let line = prefix.bytes().filter(|b| *b == b'\n').count() + 1;
        let col =
            prefix.rsplit('\n').next().unwrap_or("").chars().count() + 1;
        writeln!(out, "{}\n{file}:{line}:{col}", self.code)?;
        Report::build(ReportKind::Error, (file, self.span.clone()))
            .with_config(
                Config::default()
                    .with_color(color)
                    .with_index_type(IndexType::Byte),
            )
            .with_message(&self.message)
            .with_label(
                Label::new((file, self.span.clone()))
                    .with_message(&self.message),
            )
            .finish()
            .write((file, Source::from(source)), out)
    }
}
