//! Passes produce structured diagnostics; rendering happens only at the driver boundary.
pub mod codes;

use crate::source::{SourceMap, Span};
use ariadne::{
    Config, IndexType, Label as AriadneLabel, Report, ReportKind,
};
pub use codes::ErrorCode;
use std::io::{self, Write};

/// Errors stop a pipeline; warnings describe accepted programs.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
/// One source location, with an explicit primary-label designation.
pub struct Label {
    pub span: Span,
    pub msg: String,
    pub primary: bool,
}

#[derive(Clone, Debug)]
/// Stable identity plus contextual prose and source labels.
pub struct Diagnostic {
    pub code: ErrorCode,
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(
        code: ErrorCode,
        message: impl Into<String>,
        span: Span,
    ) -> Self {
        let message = message.into();
        Self {
            code,
            severity: Severity::Error,
            labels: vec![Label {
                span,
                msg: message.clone(),
                primary: true,
            }],
            message,
            notes: vec![],
        }
    }
    pub fn render(
        &self,
        sources: &SourceMap,
        color: bool,
        mut out: impl Write,
    ) -> io::Result<()> {
        let primary = self
            .labels
            .iter()
            .find(|l| l.primary)
            .or_else(|| self.labels.first())
            .expect("diagnostic requires a label");
        let file = sources.get(primary.span.file);
        let (line, col) = sources.line_col(primary.span);
        writeln!(
            out,
            "{}\n{}:{line}:{col}",
            self.code.as_str(),
            file.name
        )?;
        let kind = if self.severity == Severity::Error {
            ReportKind::Error
        } else {
            ReportKind::Warning
        };
        let mut report =
            Report::build(kind, (file.name.clone(), primary.span.range()))
                .with_config(
                    Config::default()
                        .with_color(color)
                        .with_index_type(IndexType::Byte),
                )
                .with_message(&self.message);
        for label in &self.labels {
            report = report.with_label(
                AriadneLabel::new((
                    sources.get(label.span.file).name.clone(),
                    label.span.range(),
                ))
                .with_message(&label.msg),
            );
        }
        for note in &self.notes {
            report = report.with_note(note);
        }
        report.finish().write(
            ariadne::sources(
                sources
                    .files
                    .iter()
                    .map(|f| (f.name.clone(), f.text.as_str())),
            ),
            out,
        )
    }
}
