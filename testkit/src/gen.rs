//! Reproducible, well-typed integer programs with an independent numeric oracle.
use crate::{Engine, Scratch, capture};
use std::{path::Path, process::Command, time::Duration};

#[derive(Clone, Debug)]
pub struct Case {
    pub source: String,
    pub expected: i64,
}

struct Generator(u64);

fn wrap(n: i128) -> i64 {
    let n = (n as u128 & ((1_u128 << 63) - 1)) as i64;
    (n << 1) >> 1
}

impl Generator {
    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.0 >> 32
    }
    fn expr(&mut self, depth: u8) -> Case {
        let n = (self.next() % 101) as i64;
        if depth == 0 {
            return Case {
                source: n.to_string(),
                expected: n,
            };
        }
        let choice = self.next() % 8;
        let a = self.expr(depth - 1);
        let b = self.expr(depth - 1);
        let (source, expected) = match choice {
            0 => (
                format!("({} + {})", a.source, b.source),
                wrap(a.expected as i128 + b.expected as i128),
            ),
            1 => (
                format!("({} * {})", a.source, b.source),
                wrap(a.expected as i128 * b.expected as i128),
            ),
            2 => (
                format!("({} - {})", a.source, b.source),
                wrap(a.expected as i128 - b.expected as i128),
            ),
            3 => (
                format!("(let {{ x = {}; }} in x + {})", a.source, b.source),
                wrap(a.expected as i128 + b.expected as i128),
            ),
            4 => (
                format!(
                    "(if {} < {} then {} else {})",
                    a.source, b.source, a.source, b.source
                ),
                a.expected.min(b.expected),
            ),
            5 => {
                let steps = n % 5;
                (
                    format!(
                        "(let {{ f k = if k == 0 then {} else f (k-1) + {}; }} in f {steps})",
                        a.source, b.source
                    ),
                    wrap(a.expected as i128 + steps as i128 * b.expected as i128),
                )
            }
            6 => (
                format!("sum [{},{},4611686018427387903]", a.source, b.source),
                wrap(a.expected as i128 + b.expected as i128 + ((1_i128 << 62) - 1)),
            ),
            _ => {
                let divisor = n % 9 + 1;
                (
                    format!("({} `div` {divisor})", a.source),
                    a.expected.div_euclid(divisor),
                )
            }
        };
        Case {
            source: format!("({source})"),
            expected,
        }
    }
}

/// Fixed seeds make a failing case reproducible without a property-test dependency.
pub fn cases(seed: u64, count: usize) -> Vec<Case> {
    let mut g = Generator(seed);
    (0..count).map(|_| g.expr(3)).collect()
}

/// Batch independent expressions to amortize Prelude compilation; every result
/// is checked against both the numeric oracle and every preceding engine.
pub fn check(binary: &Path, engines: &[Engine], seed: u64, count: usize) -> Result<(), String> {
    let dir = Scratch::new().map_err(|e| e.to_string())?;
    for (batch, cases) in cases(seed, count).chunks(20).enumerate() {
        let path = dir.0.join(format!("generated-{batch}.fun"));
        let source = format!(
            "main = do {{ {} }}\n",
            cases
                .iter()
                .map(|c| format!("print {};", c.source))
                .collect::<Vec<_>>()
                .join("\n")
        );
        let expected = cases
            .iter()
            .map(|c| format!("{}\n", c.expected))
            .collect::<String>()
            .into_bytes();
        std::fs::write(&path, &source).map_err(|e| e.to_string())?;
        let mut baseline = None;
        for engine in engines {
            let actual = if *engine == Engine::Native {
                let executable = dir.0.join(format!("native-{batch}"));
                let built = capture(
                    Command::new(binary)
                        .arg("build")
                        .arg(&path)
                        .arg("-o")
                        .arg(&executable)
                        .args(["--lint", "--color=never"]),
                    Duration::from_secs(120),
                )
                .map_err(|e| e.to_string())?;
                if built.exit != 0 {
                    return Err(format!(
                        "seed {seed} batch {batch}: build failed {built:?}\n{source}"
                    ));
                }
                capture(&mut Command::new(executable), Duration::from_secs(60))
            } else {
                capture(
                    Command::new(binary)
                        .arg("run")
                        .arg(&path)
                        .arg(format!("--engine={}", engine.name()))
                        .args(["--lint", "--color=never"]),
                    Duration::from_secs(60),
                )
            }
            .map_err(|e| e.to_string())?;
            if actual.exit != 0 || actual.stdout != expected || !actual.stderr.is_empty() {
                return Err(format!(
                    "seed {seed} batch {batch} engine {}: {actual:?}\nexpected {}\n{source}",
                    engine.name(),
                    String::from_utf8_lossy(&expected)
                ));
            }
            if let Some(previous) = &baseline
                && !actual.agrees_with(previous)
            {
                return Err(format!("seed {seed} batch {batch}: engine disagreement"));
            }
            baseline = Some(actual);
        }
    }
    Ok(())
}
