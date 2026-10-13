//! Snapshot creation and byte-for-byte corpus inheritance checks.
use crate::Result;
use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

fn chapter(root: &Path, number: u8) -> Result<PathBuf> {
    let prefix = format!("ch{number:02}_");
    let found: Vec<_> = fs::read_dir(root.join("crates"))?
        .collect::<std::io::Result<Vec<_>>>()?
        .into_iter()
        .filter(|entry| entry.file_name().to_string_lossy().starts_with(&prefix))
        .map(|entry| entry.path())
        .collect();
    match found.as_slice() {
        [path] if path.is_dir() => Ok(path.clone()),
        [] => Err(format!("chapter {number:02} does not exist").into()),
        _ => Err(format!("multiple chapter {number:02} paths").into()),
    }
}

fn files(root: &Path) -> Result<Vec<PathBuf>> {
    let mut result = Vec::new();
    for entry in fs::read_dir(root)? {
        let entry = entry?;
        if entry.file_name() == "target" {
            continue;
        }
        if entry.file_type()?.is_symlink() {
            return Err("snapshot contains symlink".into());
        }
        if entry.file_type()?.is_dir() {
            result.extend(files(&entry.path())?);
        } else {
            result.push(entry.path());
        }
    }
    result.sort();
    Ok(result)
}

/// Copy a completed snapshot, updating only Cargo identities and binary test keys.
pub fn new_chapter(root: &Path, number: u8, name: &str) -> Result<()> {
    if !(8..=24).contains(&number) {
        return Err("snapshot copying begins at chapter 08 and ends at 24".into());
    }
    if name.is_empty() || !name.bytes().all(|c| c.is_ascii_lowercase() || c == b'_') {
        return Err("chapter name must contain lowercase letters and underscores".into());
    }
    if chapter(root, number).is_ok() {
        return Err("destination chapter already exists".into());
    }
    let previous = chapter(root, number - 1)?;
    let old_name = previous
        .file_name()
        .unwrap()
        .to_str()
        .ok_or("invalid chapter name")?;
    let new_name = format!("ch{number:02}_{name}");
    let destination = root.join("crates").join(&new_name);
    let source_files = files(&previous)?;
    fs::create_dir(&destination)?;
    for source in source_files {
        let relative = source.strip_prefix(&previous)?;
        let target = destination.join(relative);
        fs::create_dir_all(target.parent().unwrap())?;
        if source.file_name().is_some_and(|s| s == "Cargo.toml")
            || source.extension().is_some_and(|s| s == "rs")
        {
            let text = fs::read_to_string(&source)?
                .replace(old_name, &new_name)
                .replace(
                    &format!("fun-ch{:02}", number - 1),
                    &format!("fun-ch{number:02}"),
                )
                .replace(
                    &format!("fun-rt-{:02}", number - 1),
                    &format!("fun-rt-{number:02}"),
                )
                .replace(
                    &format!("fun_rt_{:02}", number - 1),
                    &format!("fun_rt_{number:02}"),
                );
            fs::write(target, text)?;
        } else {
            fs::copy(source, target)?;
        }
    }
    fs::write(
        destination.join("BREAKING.md"),
        "# Changes to inherited programs\n\nNone.\n",
    )?;
    // Explicit members work before any chapter exists (empty Cargo globs do not).
    let manifest = root.join("Cargo.toml");
    let mut text = fs::read_to_string(&manifest)?;
    let start = text
        .find("members = [")
        .ok_or("workspace members list is missing")?;
    let end = start
        + text[start..]
            .find(']')
            .ok_or("workspace members list is unclosed")?;
    let prefix = text[..end].trim_end();
    let separator = if prefix.ends_with(',') || prefix.ends_with('[') {
        ""
    } else {
        ","
    };
    let mut members = format!("{separator} \"crates/{new_name}\"");
    if destination.join("rt/Cargo.toml").exists() {
        members += &format!(", \"crates/{new_name}/rt\"");
    }
    text.insert_str(end, &members);
    fs::write(manifest, text)?;
    println!("created crates/{new_name}; corpus inherited unchanged");
    Ok(())
}

/// Check every inherited source and sidecar; a breaking entry requires a reason.
fn verify_inheritance(previous: &Path, current: &Path, breaking: &str) -> Result<usize> {
    let previous_programs = previous.join("tests/programs");
    let mut changed = BTreeSet::new();
    let mut count = 0;
    for source in files(&previous_programs)? {
        if !source.extension().is_some_and(|e| e == "fun") {
            continue;
        }
        count += 1;
        let relative = source.strip_prefix(previous)?;
        let target = current.join(relative);
        if !target.exists() {
            return Err(format!("missing inherited program {}", relative.display()).into());
        }
        for extension in ["fun", "out", "err", "exit", "in"] {
            let old = read_optional(&source.with_extension(extension))?;
            let new = read_optional(&target.with_extension(extension))?;
            if old != new {
                changed.insert(relative.to_string_lossy().into_owned());
            }
        }
    }
    let mut declared = BTreeSet::new();
    for line in breaking.lines().filter_map(|l| l.strip_prefix("- ")) {
        let (path, reason) = line
            .split_once(": ")
            .ok_or("BREAKING.md entry needs path: reason")?;
        if reason.trim().is_empty() {
            return Err("BREAKING.md entry has no reason".into());
        }
        declared.insert(path.to_owned());
    }
    for path in &changed {
        if !declared.contains(path) {
            return Err(format!("undocumented inherited change: {path}").into());
        }
    }
    if let Some(path) = declared.difference(&changed).next() {
        return Err(format!("stale BREAKING.md entry: {path}").into());
    }
    Ok(count)
}

fn read_optional(path: &Path) -> Result<Option<Vec<u8>>> {
    match fs::read(path) {
        Ok(bytes) => Ok(Some(bytes)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e.into()),
    }
}

/// Inherited programs run on the default engine in the chapters between their
/// introduction and the last chapter; `full` runs every engine everywhere.
pub fn check_inherit(root: &Path, number: u8, full: bool) -> Result<()> {
    let current = chapter(root, number)?;
    if !(2..=24).contains(&number) {
        return Err("chapter must be 02 through 24".into());
    }
    let count = if number >= 3 {
        verify_inheritance(
            &chapter(root, number - 1)?,
            &current,
            &fs::read_to_string(current.join("BREAKING.md"))?,
        )?
    } else {
        0
    };
    let mut test = test_command();
    test.arg("-p").arg(current.file_name().unwrap());
    if !full && (3..24).contains(&number) {
        test.env("FUN_INHERITED_CRATE", chapter(root, number - 1)?);
    }
    if !test.status()?.success() {
        return Err("chapter tests failed".into());
    }
    println!("chapter {number:02}: {count} inherited programs verified and chapter tests passed");
    Ok(())
}

/// nextest runs a chapter's test binaries concurrently; cargo test is the fallback.
fn test_command() -> Command {
    let nextest = Command::new("cargo")
        .args(["nextest", "--version"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success());
    let mut command = Command::new("cargo");
    if nextest {
        command.args(["nextest", "run"]);
        // CI splits a late chapter across jobs with a nextest filter expression.
        if let Some(filter) = std::env::var_os("FUN_TEST_FILTER").filter(|f| !f.is_empty()) {
            command.arg("-E").arg(filter);
        }
    } else {
        command.arg("test");
    }
    command
}

/// Report implementation files, ordered by their role in the chapter.
pub fn book_diff(root: &Path, number: u8) -> Result<()> {
    print!("{}", source_diff(root, number)?);
    Ok(())
}

fn source_diff(root: &Path, number: u8) -> Result<String> {
    let current = chapter(root, number)?;
    let previous = if number >= 3 {
        Some(chapter(root, number - 1)?)
    } else {
        None
    };
    let mut new = Vec::new();
    let mut changed = Vec::new();
    for source in files(&current)? {
        let relative = source.strip_prefix(&current)?;
        if relative.extension().is_none_or(|ext| ext != "rs")
            || relative
                .components()
                .any(|part| matches!(part.as_os_str().to_str(), Some("tests" | "snapshots")))
        {
            continue;
        }
        let old = previous.as_ref().map(|dir| dir.join(relative));
        if let Some(old) = old.filter(|path| path.is_file()) {
            // Snapshot identities are mechanical changes, not chapter material.
            let old_text = fs::read_to_string(old)?
                .replace(
                    previous
                        .as_ref()
                        .unwrap()
                        .file_name()
                        .unwrap()
                        .to_str()
                        .unwrap(),
                    current.file_name().unwrap().to_str().unwrap(),
                )
                .replace(
                    &format!("fun-ch{:02}", number - 1),
                    &format!("fun-ch{number:02}"),
                )
                .replace(
                    &format!("fun-rt-{:02}", number - 1),
                    &format!("fun-rt-{number:02}"),
                )
                .replace(
                    &format!("fun_rt_{:02}", number - 1),
                    &format!("fun_rt_{number:02}"),
                );
            if old_text != fs::read_to_string(&source)? {
                changed.push(relative.to_path_buf());
            }
        } else {
            new.push(relative.to_path_buf());
        }
    }
    let mut output = String::new();
    for (title, paths) in [("New files", &mut new), ("Changed files", &mut changed)] {
        paths.sort_by_key(|path| (source_priority(number, path), path.clone()));
        output.push_str(&format!("> **{title}**\n>\n"));
        if paths.is_empty() {
            output.push_str("> None.\n");
        }
        for path in paths {
            output.push_str(&format!("> - `{}`\n", path.display()));
        }
        output.push_str(">\n");
    }
    Ok(output.trim_end_matches(">\n").to_owned())
}

fn source_priority(number: u8, path: &Path) -> usize {
    let central: &[&str] = match number {
        2 => &["lexer", "parse", "pratt", "ast", "eval"],
        3 => &["names", "normal", "eval", "ast"],
        4 => &["types", "ast", "eval"],
        5 => &["cek", "cbn", "eval", "types"],
        6 => &["algorithm_w", "union_find", "types"],
        7 => &["session", "core", "typecheck", "elaborate", "eval"],
        8 => &["layout", "fixity", "format", "parse", "typecheck"],
        9 => &["rename", "scc", "names", "renamed", "typecheck"],
        10 => &["core", "core_lint", "core_type", "elaborate", "typecheck"],
        11 => &["data", "syn", "desugar", "typecheck", "core", "elaborate"],
        12 => &["typecheck", "data", "lexer", "elaborate", "ast"],
        13 => &["kinds", "types", "typecheck", "core_lint"],
        14 => &[
            "classes",
            "evidence",
            "dictionaries",
            "typecheck",
            "elaborate",
            "types",
        ],
        15 => &["io_lower", "desugar", "eval", "typecheck", "classes"],
        16 => &["match_compile", "usefulness", "core", "core_lint", "eval"],
        17 => &["derive", "primitives", "desugar", "typecheck"],
        18 => &["erase", "anf", "anf_simplify", "anf_machine", "anf_lint"],
        19 => &["closure", "flat", "flat_machine", "flat_lint"],
        20 => &["layout", "abi", "heap", "apply", "primitives"],
        21 => &["mod", "control", "values", "primitives", "native", "flat"],
        22 => &["jit", "repl", "mod", "control", "heap", "session", "rename"],
        23 => &[
            "rep",
            "reach",
            "erase",
            "values",
            "primitives",
            "anf_simplify",
            "dump",
        ],
        24 => &[
            "copying", "liveness", "roots", "control", "values", "heap", "native",
        ],
        _ => &[],
    };
    let stem = path
        .file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    central
        .iter()
        .position(|name| *name == stem)
        .map_or(100, |index| index + 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_summary_filters_noise_and_prioritizes_chapter_modules() {
        let root = std::env::temp_dir().join(format!("fun-source-diff-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        let old = root.join("crates/ch13_kinds");
        let new = root.join("crates/ch14_classes");
        for dir in [&old, &new] {
            fs::create_dir_all(dir.join("src")).unwrap();
            fs::create_dir_all(dir.join("tests/snapshots")).unwrap();
        }
        fs::write(old.join("src/main.rs"), "use ch13_kinds::run; // fun-ch13").unwrap();
        fs::write(
            new.join("src/main.rs"),
            "use ch14_classes::run; // fun-ch14",
        )
        .unwrap();
        fs::write(old.join("src/typecheck.rs"), "old").unwrap();
        fs::write(new.join("src/typecheck.rs"), "new").unwrap();
        for file in [
            "src/classes.rs",
            "src/ast.rs",
            "tests/test.rs",
            "tests/snapshots/dump.rs",
            "src/grammar.lalrpop",
        ] {
            fs::write(new.join(file), "new").unwrap();
        }
        assert_eq!(
            source_diff(&root, 14).unwrap(),
            "> **New files**\n>\n> - `src/classes.rs`\n> - `src/ast.rs`\n>\n> **Changed files**\n>\n> - `src/typecheck.rs`\n"
        );
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn snapshot_updates_source_identities_and_preserves_corpus() {
        let root = std::env::temp_dir().join(format!("fun-copy-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        let old = root.join("crates/ch07_fun");
        fs::create_dir_all(old.join("src")).unwrap();
        fs::create_dir_all(old.join("tests/programs")).unwrap();
        fs::write(root.join("Cargo.toml"), "[workspace]\nmembers = [\"crates/ch07_fun\"]\nexclude = [\".references\"]\nresolver = \"3\"\n").unwrap();
        fs::write(old.join("Cargo.toml"), "name = \"ch07_fun\"\n").unwrap();
        fs::write(
            old.join("src/main.rs"),
            "use ch07_fun::Session; // fun-ch07",
        )
        .unwrap();
        fs::write(old.join("tests/programs/test.fun"), "{ main = 42; }").unwrap();
        new_chapter(&root, 8, "layout").unwrap();
        let new = root.join("crates/ch08_layout");
        assert_eq!(
            fs::read_to_string(new.join("src/main.rs")).unwrap(),
            "use ch08_layout::Session; // fun-ch08"
        );
        assert!(
            fs::read_to_string(root.join("Cargo.toml"))
                .unwrap()
                .contains("\"crates/ch08_layout\"]")
        );
        assert_eq!(verify_inheritance(&old, &new, "None").unwrap(), 1);
        assert!(new_chapter(&root, 8, "layout").is_err());
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn snapshot_accepts_formatted_member_arrays() {
        let root = std::env::temp_dir().join(format!("fun-formatted-copy-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(root.join("crates/ch07_fun/src")).unwrap();
        fs::write(
            root.join("Cargo.toml"),
            "[workspace]\nmembers = [\n  \"crates/ch07_fun\",\n]\n",
        )
        .unwrap();
        fs::write(
            root.join("crates/ch07_fun/Cargo.toml"),
            "name = \"ch07_fun\"\n",
        )
        .unwrap();
        new_chapter(&root, 8, "layout").unwrap();
        let manifest = fs::read_to_string(root.join("Cargo.toml")).unwrap();
        assert!(!manifest.replace(char::is_whitespace, "").contains(",,"));
        assert!(manifest.contains("crates/ch08_layout"));
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn sidecar_changes_require_breaking_entry() {
        let root = std::env::temp_dir().join(format!("fun-inherit-{}", std::process::id()));
        let _ = fs::remove_dir_all(&root);
        for name in ["old", "new"] {
            let dir = root.join(name).join("tests/programs");
            fs::create_dir_all(&dir).unwrap();
            fs::write(dir.join("test.fun"), "main = 1").unwrap();
            fs::write(dir.join("test.out"), "1\n").unwrap();
        }
        let old = root.join("old");
        let new = root.join("new");
        assert_eq!(verify_inheritance(&old, &new, "None").unwrap(), 1);
        fs::write(new.join("tests/programs/test.exit"), "1").unwrap();
        assert!(verify_inheritance(&old, &new, "None").is_err());
        assert!(
            verify_inheritance(
                &old,
                &new,
                "- tests/programs/test.fun: new expectation; test reason."
            )
            .is_ok()
        );
        fs::remove_dir_all(root).unwrap();
    }
}
