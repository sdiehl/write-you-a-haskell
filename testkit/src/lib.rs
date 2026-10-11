//! Golden corpus runner shared by independently runnable compiler snapshots.
//! Expectations are bytes. Only the first stderr line participates in the
//! differential comparison; an optional second golden line checks source position.
//! A `-- stderr: exact` header instead compares all stderr bytes, including traces.
use std::ffi::OsStr;
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

/// An execution backend understood by the chapter's command line.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Engine {
    Core,
    Cek,
    Anf,
    Flat,
    Jit,
    Native,
}
impl Engine {
    pub fn name(self) -> &'static str {
        match self {
            Self::Core => "core",
            Self::Cek => "cek",
            Self::Anf => "anf",
            Self::Flat => "flat",
            Self::Jit => "jit",
            Self::Native => "native",
        }
    }
}

// ANCHOR: config
/// The chapter supplies its binary and available engines; the runner owns policy.
pub struct Config<'a> {
    pub binary: &'a Path,
    pub programs: &'a Path,
    pub engines: &'a [Engine],
    pub timeout: Duration,
}
// ANCHOR_END: config

/// The observable behavior of one process, without terminal formatting.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Outcome {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
    pub exit: i32,
}
impl Outcome {
    fn first_error(&self) -> &[u8] {
        self.stderr.split(|b| *b == b'\n').next().unwrap_or(&[])
    }
    /// Ignore diagnostic elaboration, but never ignore stdout bytes or exit status.
    pub fn agrees_with(&self, other: &Self) -> bool {
        self.stdout == other.stdout
            && self.first_error() == other.first_error()
            && self.exit == other.exit
    }
}

/// A program and the expectations stored alongside it.
#[derive(Debug)]
pub struct Fixture {
    pub path: PathBuf,
    pub stdout: Vec<u8>,
    pub input: Option<Vec<u8>>,
    pub error: Option<Vec<u8>>,
    pub exit: i32,
    pub engines: Option<Vec<String>>,
    pub exact_stderr: bool,
}
impl Fixture {
    pub fn read(path: PathBuf) -> io::Result<Self> {
        let source = fs::read_to_string(&path)?;
        let error = optional_read(&path.with_extension("err"))?;
        let exit = match optional_read(&path.with_extension("exit"))? {
            Some(bytes) => String::from_utf8_lossy(&bytes)
                .trim()
                .parse()
                .map_err(|_| io::Error::other(format!("{}: invalid exit code", path.display())))?,
            None => i32::from(error.is_some()),
        };
        let engines = source
            .lines()
            .take_while(|line| line.trim().is_empty() || line.trim_start().starts_with("--"))
            .find_map(|line| {
                line.trim().strip_prefix("-- engines:").map(|rest| {
                    rest.split_whitespace()
                        .map(str::to_owned)
                        .collect::<Vec<_>>()
                })
            });
        if let Some(names) = &engines
            && (names.is_empty()
                || names.iter().any(|n| {
                    !["core", "cek", "anf", "flat", "jit", "native"].contains(&n.as_str())
                }))
        {
            return Err(io::Error::other(format!(
                "{}: invalid engines header",
                path.display()
            )));
        }
        // Empty output still needs an explicit .out: a missing expectation is a bug.
        let stdout = fs::read(path.with_extension("out"))?;
        let input = optional_read(&path.with_extension("in"))?;
        let exact_stderr = source
            .lines()
            .take_while(|line| line.trim().is_empty() || line.trim_start().starts_with("--"))
            .any(|line| line.trim() == "-- stderr: exact");
        Ok(Self {
            exact_stderr,
            input,
            path,
            stdout,
            error,
            exit,
            engines,
        })
    }

    pub fn compare(&self, actual: &Outcome) -> Result<(), String> {
        if actual.stdout != self.stdout || actual.exit != self.exit {
            return Err(format!(
                "{}: expected stdout {:?}, exit {}; got {:?}, exit {}",
                self.path.display(),
                String::from_utf8_lossy(&self.stdout),
                self.exit,
                String::from_utf8_lossy(&actual.stdout),
                actual.exit
            ));
        }
        // Expand only expectations; actual output and differential comparisons remain bytes.
        let expected = self.error.as_deref().unwrap_or_default();
        let mut expanded = Vec::new();
        let marker = b"{FILE}";
        let mut rest = expected;
        while let Some(i) = rest.windows(marker.len()).position(|w| w == marker) {
            expanded.extend_from_slice(&rest[..i]);
            expanded.extend_from_slice(self.path.as_os_str().as_encoded_bytes());
            rest = &rest[i + marker.len()..];
        }
        expanded.extend_from_slice(rest);
        if self.exact_stderr {
            return if actual.stderr == expanded {
                Ok(())
            } else {
                Err(format!(
                    "{}: full stderr differs: expected {:?}, got {:?}",
                    self.path.display(),
                    String::from_utf8_lossy(&expanded),
                    String::from_utf8_lossy(&actual.stderr)
                ))
            };
        }
        let mut lines = expanded.split(|b| *b == b'\n');
        let first = lines.next().unwrap_or_default();
        if actual.first_error() != first {
            return Err(format!(
                "{}: expected first stderr line {:?}; got {:?}",
                self.path.display(),
                String::from_utf8_lossy(first),
                String::from_utf8_lossy(actual.first_error())
            ));
        }
        if let Some(location) = lines.next().filter(|line| !line.is_empty()) {
            let actual_location = actual
                .stderr
                .split(|b| *b == b'\n')
                .nth(1)
                .unwrap_or_default();
            if actual_location != location {
                return Err(format!(
                    "{}: expected primary location {:?}; got {:?}",
                    self.path.display(),
                    String::from_utf8_lossy(location),
                    String::from_utf8_lossy(actual_location)
                ));
            }
        }
        Ok(())
    }
}

fn optional_read(path: &Path) -> io::Result<Option<Vec<u8>>> {
    match fs::read(path) {
        Ok(bytes) => Ok(Some(bytes)),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

/// Discover in deterministic path order and reject symlinks to avoid corpus escapes.
pub fn discover(root: &Path) -> io::Result<Vec<PathBuf>> {
    let mut result = Vec::new();
    for entry in fs::read_dir(root)? {
        let entry = entry?;
        let kind = entry.file_type()?;
        if kind.is_symlink() {
            return Err(io::Error::other("symlinks are not allowed in a corpus"));
        } else if kind.is_dir() {
            result.extend(discover(&entry.path())?);
        } else if entry.path().extension() == Some(OsStr::new("fun")) {
            result.push(entry.path());
        }
    }
    result.sort();
    Ok(result)
}

/// Run a child with concurrently drained pipes and a bounded execution time.
/// No shell interprets program paths or arguments. Closed stdin makes EOF repeatable.
pub fn capture(command: &mut Command, timeout: Duration) -> io::Result<Outcome> {
    capture_with_input(command, timeout, None)
}
/// Feed optional fixture input while draining both output pipes independently.
pub fn capture_with_input(
    command: &mut Command,
    timeout: Duration,
    input: Option<&[u8]>,
) -> io::Result<Outcome> {
    let mut child = command
        .stdin(if input.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let writer = input.map(|bytes| {
        let bytes = bytes.to_vec();
        let mut stdin = child.stdin.take().expect("stdin pipe");
        thread::spawn(move || stdin.write_all(&bytes))
    });
    let mut stdout = child
        .stdout
        .take()
        .ok_or_else(|| io::Error::other("missing stdout pipe"))?;
    let mut stderr = child
        .stderr
        .take()
        .ok_or_else(|| io::Error::other("missing stderr pipe"))?;
    let out = thread::spawn(move || {
        let mut bytes = Vec::new();
        stdout.read_to_end(&mut bytes).map(|_| bytes)
    });
    let err = thread::spawn(move || {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes).map(|_| bytes)
    });
    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait()? {
            break status;
        }
        if start.elapsed() >= timeout {
            child.kill()?;
            child.wait()?;
            let _ = out.join();
            let _ = err.join();
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                format!("{command:?} exceeded {timeout:?}"),
            ));
        }
        thread::sleep(Duration::from_millis(10));
    };
    if let Some(writer) = writer {
        match writer
            .join()
            .map_err(|_| io::Error::other("stdin writer panicked"))?
        {
            Err(e) if e.kind() != io::ErrorKind::BrokenPipe => return Err(e),
            _ => {}
        }
    }
    Ok(Outcome {
        stdout: out
            .join()
            .map_err(|_| io::Error::other("stdout reader panicked"))??,
        stderr: err
            .join()
            .map_err(|_| io::Error::other("stderr reader panicked"))??,
        exit: status.code().unwrap_or(-1),
    })
}

/// Run every golden fixture and compare all requested, available engines.
/// A compile-time error is checked once; accepted programs run on every engine.
pub fn run_corpus(config: &Config<'_>) -> Result<usize, String> {
    corpus(config, false)
}
/// Repeat native-eligible fixtures with relocation forced at every allocation.
pub fn run_copying_corpus(config: &Config<'_>) -> Result<usize, String> {
    corpus(config, true)
}
fn corpus(config: &Config<'_>, copying: bool) -> Result<usize, String> {
    if config.engines.is_empty() {
        return Err("no engines configured".into());
    }
    let paths = discover(config.programs).map_err(|e| e.to_string())?;
    if paths.is_empty() {
        return Err("corpus is empty".into());
    }
    let scratch = Scratch::new().map_err(|e| e.to_string())?;
    let workers = thread::available_parallelism()
        .map_or(1, usize::from)
        .min(4);
    let next = std::sync::atomic::AtomicUsize::new(0);
    let results = thread::scope(|scope| {
        let next = &next;
        let paths = &paths;
        let scratch = &scratch;
        let handles = (0..workers)
            .map(|_| {
                scope.spawn(move || {
                    let mut results = Vec::new();
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(path) = paths.get(i) else { break };
                        results.push((i, run_fixture(config, scratch, i, path, copying)));
                    }
                    results
                })
            })
            .collect::<Vec<_>>();
        handles
            .into_iter()
            .flat_map(|h| h.join().expect("corpus worker panicked"))
            .collect::<Vec<_>>()
    });
    let mut results = results;
    results.sort_by_key(|(i, _)| *i);
    for (_, result) in results {
        result?;
    }
    Ok(paths.len())
}
fn run_fixture(
    config: &Config<'_>,
    scratch: &Scratch,
    i: usize,
    path: &PathBuf,
    copying: bool,
) -> Result<(), String> {
    let fixture = Fixture::read(path.clone()).map_err(|e| format!("{}: {e}", path.display()))?;
    if copying
        && fixture
            .engines
            .as_ref()
            .is_some_and(|es| !es.iter().any(|e| e == "native"))
    {
        return Ok(());
    }
    let mut check = Command::new(config.binary);
    check
        .arg("check")
        .arg(path)
        .args(["--lint", "--color=never"]);
    let checked = capture(&mut check, config.timeout).map_err(|e| e.to_string())?;
    if checked.exit != 0 {
        fixture.compare(&checked)?;
        return Ok(());
    }
    let mut baseline: Option<Outcome> = None;
    for &engine in config.engines {
        if fixture
            .engines
            .as_ref()
            .is_some_and(|names| !names.iter().any(|n| n == engine.name()))
        {
            continue;
        }
        let actual = if engine == Engine::Native {
            let executable = scratch.0.join(format!("program-{i}"));
            let mut build = Command::new(config.binary);
            build
                .arg("build")
                .arg(path)
                .arg("-o")
                .arg(&executable)
                .args(["--lint", "--color=never"]);
            if copying {
                build.arg("--gc=copying");
            }
            let built = capture(&mut build, config.timeout).map_err(|e| e.to_string())?;
            if built.exit != 0 {
                return Err(format!(
                    "{}: native build failed: {:?}",
                    path.display(),
                    built
                ));
            }
            let mut execute = Command::new(executable);
            if copying {
                execute.env("FUN_GC_STRESS", "1");
            }
            let mut executed =
                capture_with_input(&mut execute, config.timeout, fixture.input.as_deref())
                    .map_err(|e| e.to_string())?;
            // Interpreters report warnings after execution; runtime failures take precedence.
            // Native diagnostics arrive during build, before the separate executable runs.
            if executed.stderr.is_empty() {
                executed.stderr = built.stderr;
            }
            Ok(executed)
        } else {
            let mut run = Command::new(config.binary);
            run.arg("run")
                .arg(path)
                .arg(format!("--engine={}", engine.name()))
                .args(["--lint", "--color=never"]);
            capture_with_input(&mut run, config.timeout, fixture.input.as_deref())
        }
        .map_err(|e| e.to_string())?;
        fixture
            .compare(&actual)
            .map_err(|e| format!("{}: {e}", engine.name()))?;
        if let Some(previous) = &baseline {
            if !previous.agrees_with(&actual) {
                return Err(format!("{}: engine disagreement", path.display()));
            }
        } else {
            baseline = Some(actual);
        }
    }
    if baseline.is_none() {
        return Err(format!("{}: no requested engine available", path.display()));
    }
    Ok(())
}

struct Scratch(PathBuf);
impl Scratch {
    fn new() -> io::Result<Self> {
        for i in 0..1000 {
            let path = std::env::temp_dir().join(format!("fun-testkit-{}-{i}", std::process::id()));
            match fs::create_dir(&path) {
                Ok(()) => return Ok(Self(path)),
                Err(e) if e.kind() == io::ErrorKind::AlreadyExists => continue,
                Err(e) => return Err(e),
            }
        }
        Err(io::Error::other("cannot allocate test directory"))
    }
}
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn fixture_checks_bytes_exit_and_primary_location() {
        let dir = Scratch::new().unwrap();
        let path = dir.0.join("bad.fun");
        fs::write(&path, "-- engines: core anf\nmain = x").unwrap();
        fs::write(path.with_extension("out"), "").unwrap();
        fs::write(path.with_extension("err"), "E0101\nbad.fun:2:8\n").unwrap();
        let fixture = Fixture::read(path).unwrap();
        assert_eq!(fixture.exit, 1);
        let mut actual = Outcome {
            stdout: vec![],
            stderr: b"E0101\nbad.fun:2:8\nmore context\n".to_vec(),
            exit: 1,
        };
        assert!(fixture.compare(&actual).is_ok());
        actual.stderr = b"E0101\nbad.fun:2:9\n".to_vec();
        assert!(fixture.compare(&actual).is_err());
        actual.exit = 0;
        assert!(fixture.compare(&actual).is_err());
    }
    #[test]
    fn exact_stderr_checks_every_trace_line() {
        let fixture = Fixture {
            path: "trace.fun".into(),
            input: None,
            stdout: vec![],
            error: Some(b"first\nsecond\n".to_vec()),
            exit: 0,
            engines: None,
            exact_stderr: true,
        };
        assert!(
            fixture
                .compare(&Outcome {
                    stdout: vec![],
                    stderr: b"first\nsecond\n".to_vec(),
                    exit: 0
                })
                .is_ok()
        );
        assert!(
            fixture
                .compare(&Outcome {
                    stdout: vec![],
                    stderr: b"first\nwrong\n".to_vec(),
                    exit: 0
                })
                .is_err()
        );
    }
    #[test]
    fn file_marker_expands_only_expected_diagnostics() {
        let fixture = Fixture {
            exact_stderr: false,
            input: None,
            path: "nested/failure.fun".into(),
            stdout: vec![],
            error: Some(b"fun: pattern match failure at {FILE}:1:8\n".to_vec()),
            exit: 1,
            engines: None,
        };
        let actual = Outcome {
            stdout: vec![],
            stderr: b"fun: pattern match failure at nested/failure.fun:1:8\n".to_vec(),
            exit: 1,
        };
        assert!(fixture.compare(&actual).is_ok());
        let wrong = Outcome {
            stderr: b"fun: pattern match failure at other.fun:1:8\n".to_vec(),
            ..actual
        };
        assert!(fixture.compare(&wrong).is_err());
    }
    #[test]
    fn discovery_is_recursive_and_sorted() {
        let dir = Scratch::new().unwrap();
        fs::create_dir(dir.0.join("nested")).unwrap();
        fs::write(dir.0.join("z.fun"), "").unwrap();
        fs::write(dir.0.join("nested/a.fun"), "").unwrap();
        fs::write(dir.0.join("ignore.out"), "").unwrap();
        let found = discover(&dir.0).unwrap();
        assert_eq!(found.len(), 2);
        assert!(found[0].ends_with("nested/a.fun"));
    }
    #[test]
    fn process_drains_large_pipes_and_times_out() {
        let mut command = Command::new("sh");
        command.args([
            "-c",
            "i=0; while [ $i -lt 10000 ]; do echo output; echo error >&2; i=$((i+1)); done",
        ]);
        let outcome = capture(&mut command, Duration::from_secs(10)).unwrap();
        assert_eq!(outcome.stdout.len(), 70000);
        assert_eq!(outcome.stderr.len(), 60000);
        let mut command = Command::new("sleep");
        command.arg("5");
        assert_eq!(
            capture(&mut command, Duration::from_millis(20))
                .unwrap_err()
                .kind(),
            io::ErrorKind::TimedOut
        );
    }
}

#[test]
fn input_and_output_pipes_do_not_deadlock() {
    let bytes = vec![b'x'; 256 * 1024];
    let result = capture_with_input(
        &mut Command::new("cat"),
        Duration::from_secs(5),
        Some(&bytes),
    )
    .unwrap();
    assert_eq!(result.stdout, bytes);
    assert_eq!(result.exit, 0);
}

#[path = "gen.rs"]
pub mod generate;
