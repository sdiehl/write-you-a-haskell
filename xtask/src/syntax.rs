//! Both book highlighters are generated from the compiler's lexical tables.
use crate::Result;
use std::{fs, path::Path};

/// Write generated syntax definitions, or verify checked-in outputs in CI.
pub fn generate(root: &Path, check: bool) -> Result<()> {
    let lexer = fs::read_to_string(root.join("crates/ch07_fun/src/lexer.rs"))?;
    let words = lexer
        .split_once("pub const KEYWORDS")
        .ok_or("missing keyword table")?
        .1
        .split_once("];")
        .ok_or("unclosed keyword table")?
        .0;
    let words: Vec<_> = words.split('"').skip(1).step_by(2).collect();
    let symbols = lexer
        .split_once("pub const SYMBOLS")
        .ok_or("missing operator table")?
        .1
        .split('"')
        .nth(1)
        .ok_or("missing operator string")?
        .replace("\\\\", "\\");
    let class = symbols
        .chars()
        .map(|c| {
            if matches!(c, '\\' | '[' | ']' | '^' | '-') {
                format!("\\{c}")
            } else {
                c.to_string()
            }
        })
        .collect::<String>();
    for (template, destination, keywords, operators) in [
        (
            "fun-highlight.js",
            "book/theme/fun-highlight.js",
            words.join(" "),
            class.replace('\\', "\\\\").replace('\'', "\\'"),
        ),
        (
            "fun.sublime-syntax",
            "pdf/fun.sublime-syntax",
            words.join("|"),
            class.replace('\'', "''"),
        ),
    ] {
        let source = fs::read_to_string(root.join("xtask/templates").join(template))?;
        let output = source
            .replace("@KEYWORDS@", &keywords)
            .replace("@SYMBOLS@", &operators);
        let destination = root.join(destination);
        if check {
            if fs::read_to_string(&destination)? != output {
                return Err(
                    format!("{} is stale; run cargo xtask syntax", destination.display()).into(),
                );
            }
        } else {
            fs::write(destination, output)?;
        }
    }
    println!(
        "Fun HTML and PDF syntax tables {}",
        if check { "verified" } else { "generated" }
    );
    Ok(())
}
