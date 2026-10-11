use std::{
    path::{Path, PathBuf},
    process::Command,
    time::Duration,
};
use testkit::{Engine, Fixture, Outcome};
fn binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_fun-ch23"))
}
fn programs() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/programs")
}
fn run(fixture: &Fixture, simplify: bool) -> (Outcome, usize) {
    let mut command = Command::new(binary());
    command.arg("run").arg(&fixture.path).args([
        "--engine=anf",
        "--lint",
        "--print-steps",
    ]);
    if !simplify {
        command.arg("--no-simplify");
    }
    let mut outcome = testkit::capture_with_input(
        &mut command,
        Duration::from_secs(120),
        fixture.input.as_deref(),
    )
    .unwrap();
    assert_eq!(
        outcome.exit,
        0,
        "{}: {}",
        fixture.path.display(),
        String::from_utf8_lossy(&outcome.stderr)
    );
    let text = String::from_utf8(outcome.stderr).unwrap();
    let (trace, steps) = text
        .rsplit_once("steps: ")
        .expect("successful ANF run reports work");
    let (number, after) =
        steps.split_once('\n').expect("step line terminator");
    let steps = number.trim().parse().unwrap();
    outcome.stderr = format!("{trace}{after}").into_bytes();
    (outcome, steps)
}
#[test]
fn simplification_preserves_observations_and_does_not_increase_work() {
    let mut checked = 0;
    for path in testkit::discover(&programs()).unwrap() {
        let f = Fixture::read(path).unwrap();
        if f.exit != 0
            || f.engines
                .as_ref()
                .is_some_and(|es| !es.iter().any(|e| e == "anf"))
        {
            continue;
        }
        let (before, old_steps) = run(&f, false);
        let (after, new_steps) = run(&f, true);
        assert_eq!(before, after, "{}", f.path.display());
        f.compare(&after)
            .unwrap_or_else(|e| panic!("{}: {e}", f.path.display()));
        assert!(
            new_steps <= old_steps,
            "{}: {new_steps} > {old_steps}",
            f.path.display()
        );
        checked += 1;
    }
    assert!(
        checked > 200,
        "unexpectedly small oracle coverage: {checked}"
    );
    eprintln!("work oracle: {checked} successful ANF fixtures");
}
#[test]
fn optimization_programs_agree_on_every_engine() {
    testkit::run_corpus(&testkit::Config {
        binary: binary(),
        programs: &programs().join("optimize"),
        engines: &[
            Engine::Core,
            Engine::Anf,
            Engine::Flat,
            Engine::Native,
            Engine::Jit,
        ],
        timeout: Duration::from_secs(120),
    })
    .unwrap();
}
#[test]
fn reachability_preserves_failure_and_explains_function_dependencies() {
    let directory = tempfile::tempdir().unwrap();
    for (name, source) in [
        (
            "chain",
            "leaf x = x\nmid x = leaf x\ndead x = x\nmain = print (mid 42)\n",
        ),
        (
            "failure",
            "unused = error \"initializer\" :: Int\nmain = putStrLn \"hello\"\n",
        ),
    ] {
        let path = directory.path().join(format!("{name}.fun"));
        std::fs::write(&path, source).unwrap();
        let outcome = testkit::capture(
            Command::new(binary())
                .args(["dump", "--pass=reach"])
                .arg(&path)
                .arg("--lint"),
            Duration::from_secs(30),
        )
        .unwrap();
        assert_eq!(outcome.exit, 0, "{:?}", outcome);
        let text = String::from_utf8(outcome.stdout).unwrap();
        if name == "chain" {
            assert!(text.contains("leaf <- mid"));
            assert!(!text.lines().any(|l| l.starts_with("dead <-")));
        } else {
            assert!(text.contains("unused <- root"));
            let run = testkit::capture(
                Command::new(binary())
                    .arg("run")
                    .arg(&path)
                    .args(["--engine=anf", "--lint"]),
                Duration::from_secs(30),
            )
            .unwrap();
            assert_eq!(run.exit, 1);
            assert!(run.stdout.is_empty());
            assert_eq!(run.stderr, b"fun: initializer\n");
        }
        insta::assert_snapshot!(format!("reach-{name}"), text);
    }
}

fn lower(
    source: &str,
) -> (ch23_optimize::session::Session, ch23_optimize::anf::Program) {
    use ch23_optimize::{
        anf, elaborate, erase, match_compile, parse, session::Session,
        typecheck,
    };
    let mut sess = Session::default();
    let file = sess.sources.add("optimization.fun", source.into());
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    let core = match_compile::compile(&mut sess, core).unwrap();
    let anf = anf::convert(&mut sess, erase::erase(core));
    (sess, anf)
}
#[test]
fn missing_or_contradictory_numeric_evidence_is_rejected() {
    let (_, mut p) = lower("main = 1 + 2");
    ch23_optimize::anf_lint::lint(&p).unwrap();
    let id = p
        .reps
        .iter()
        .find(|(_, r)| **r == ch23_optimize::rep::Rep::Int)
        .unwrap()
        .0
        .clone();
    p.reps.remove(&id);
    assert!(
        ch23_optimize::anf_lint::lint(&p)
            .unwrap_err()
            .contains("missing representation")
    );
    // Every primitive Int operand now falsely claims Double evidence.
    for rep in p.reps.values_mut() {
        if *rep == ch23_optimize::rep::Rep::Int {
            *rep = ch23_optimize::rep::Rep::Double;
        }
    }
    p.reps.insert(id, ch23_optimize::rep::Rep::Double);
    assert!(
        ch23_optimize::anf_lint::lint(&p)
            .unwrap_err()
            .contains("contradictory")
    );
}
#[test]
fn a_lambda_used_twice_is_not_beta_reduced() {
    use ch23_optimize::{
        anf::{Anf, Atom, Lambda, Rhs},
        anf_lint, anf_simplify,
        rep::Rep,
    };
    use std::rc::Rc;
    let (mut sess, mut p) = lower("main = 42");
    let main = p.metadata.main.name.clone();
    let f = sess.fresh_id("f");
    let x = sess.fresh_id("x");
    let a = sess.fresh_id("a");
    let b = sess.fresh_id("b");
    p.reps.extend([
        (f.clone(), Rep::Word),
        (x.clone(), Rep::Int),
        (a.clone(), Rep::Int),
        (b.clone(), Rep::Int),
    ]);
    let lambda = Lambda {
        params: vec![x.clone()],
        body: Rc::new(Anf::Ret(Atom::Var(x))),
    };
    let body = Rc::new(Anf::Let(
        f.clone(),
        Rhs::Lambda(lambda),
        Rc::new(Anf::Let(
            a,
            Rhs::Call(
                Atom::Var(f.clone()),
                vec![Atom::Lit(ch23_optimize::core::Lit::Int(21))],
            ),
            Rc::new(Anf::Let(
                b.clone(),
                Rhs::Call(
                    Atom::Var(f.clone()),
                    vec![Atom::Lit(ch23_optimize::core::Lit::Int(42))],
                ),
                Rc::new(Anf::Ret(Atom::Var(b))),
            )),
        )),
    ));
    p.globals = vec![(main, body)];
    anf_simplify::simplify(&mut sess, &mut p);
    anf_lint::lint(&p).unwrap();
    let mut calls = 0;
    let mut lambdas = 0;
    let mut work = vec![p.globals[0].1.as_ref()];
    while let Some(body) = work.pop() {
        if let Anf::Let(_, rhs, rest) = body {
            match rhs {
                Rhs::Lambda(l) => {
                    lambdas += 1;
                    work.push(&l.body);
                }
                Rhs::Call(Atom::Var(v), _) if *v == f => calls += 1,
                _ => {}
            }
            work.push(rest);
        }
    }
    assert_eq!((lambdas, calls), (1, 2));
}
