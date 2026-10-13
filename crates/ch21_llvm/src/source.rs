//! Stable file identities and byte spans, independent of diagnostics rendering.
use std::ops::Range;

// ANCHOR: source
/// Index of a source file in a session's source map.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

/// Half-open byte offsets within one source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub file: FileId,
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(file: FileId, start: usize, end: usize) -> Self {
        Self {
            file,
            start: start.try_into().expect("source exceeds 4 GiB"),
            end: end.try_into().expect("source exceeds 4 GiB"),
        }
    }
    pub fn range(self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
    pub fn through(self, other: Self) -> Self {
        assert_eq!(self.file, other.file);
        Self {
            end: other.end,
            ..self
        }
    }
}

/// Original text and line starts; columns count Unicode scalar values.
pub struct SourceFile {
    pub name: String,
    pub text: String,
    lines: Vec<usize>,
}

#[derive(Default)]
/// Owns every source used by this compilation, including future Prelude input.
pub struct SourceMap {
    pub files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn add(&mut self, name: impl Into<String>, text: String) -> FileId {
        assert!(u32::try_from(text.len()).is_ok(), "source exceeds 4 GiB");
        let mut lines = vec![0];
        lines.extend(text.match_indices('\n').map(|(i, _)| i + 1));
        let id = FileId(
            self.files.len().try_into().expect("too many source files"),
        );
        self.files.push(SourceFile {
            name: name.into(),
            text,
            lines,
        });
        id
    }
    pub fn get(&self, file: FileId) -> &SourceFile {
        &self.files[file.0 as usize]
    }
    pub fn line_col(&self, span: Span) -> (usize, usize) {
        let file = self.get(span.file);
        let offset = span.start as usize;
        let line = file.lines.partition_point(|start| *start <= offset) - 1;
        (
            line + 1,
            file.text[file.lines[line]..offset].chars().count() + 1,
        )
    }
}
// ANCHOR_END: source
