//! Collect only preprocessed Markdown, in the author's SUMMARY order.
use crate::Result;
use std::{fs, path::Path};

/// Concatenate chapter-scoped Divs so filters can namespace repeated headings.
pub fn collect(root: &Path) -> Result<()> {
    let summary = fs::read_to_string(root.join("book/src/SUMMARY.md"))?;
    let mut book = String::from(
        "```{=typst}\n#import \"../../pdf/template/book.typ\": part, chapter, appendix, callout\n```\n",
    );
    let mut chapters = 0;
    for line in summary.lines() {
        if let Some(part) = line.strip_prefix("# ").filter(|p| *p != "Summary") {
            book.push_str(&format!("\n::: {{.part}}\n{part}\n:::\n\n"));
        }
        if let Some((_, rest)) = line.split_once("](") {
            let path = rest.strip_suffix(')').ok_or("invalid SUMMARY link")?;
            if path.contains("..") || path.starts_with('/') || !path.ends_with(".md") {
                return Err(format!("invalid chapter path: {path}").into());
            }
            let source = root.join("book/build/markdown").join(path);
            let text = fs::read_to_string(&source)
                .map_err(|e| format!("{}: {e}; run mdbook build first", source.display()))?;
            if text.contains("{{#include")
                || text.contains("#![function!")
                || text.contains("#![source_file!")
            {
                return Err(format!("unresolved include in {path}").into());
            }
            let chapter = path.trim_end_matches(".md");
            book.push_str(&format!(
                "\n::: {{.chapter-source data-chapter=\"{chapter}\"}}\n{text}\n:::\n\n"
            ));
            chapters += 1;
        }
    }
    if chapters == 0 {
        return Err("SUMMARY contains no chapters".into());
    }
    fs::create_dir_all(root.join("build/pdf"))?;
    fs::write(root.join("build/pdf/book.md"), book)?;
    println!("collected {chapters} chapters from preprocessed Markdown");
    Ok(())
}
