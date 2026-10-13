use ch03_lambda::{
    eval,
    names::{self, Term},
    normal, parse,
};

fn cbv(source: &str, fuel: usize) -> Result<String, String> {
    eval::evaluate(&parse(source).unwrap(), &vec![], &mut fuel.clone())
        .map(|v| v.to_string())
}

fn normal(source: &str) -> String {
    normal::reduce(parse(source).unwrap(), 10000, |_| {})
        .unwrap()
        .to_string()
}

#[test]
fn closures_capture_definition_scope() {
    assert_eq!(
        cbv(
            "let x = 1 in let f = \\y -> x + y in let x = 100 in f 2",
            200
        )
        .unwrap(),
        "3"
    );
    assert_eq!(cbv("(\\f -> f 4) (\\x -> x * x)", 200).unwrap(), "16");
    assert_eq!(cbv("if True then 3 else 1 / 0", 200).unwrap(), "3");
    assert_eq!(
        cbv("(\\x -> 1) (1 / 0)", 200).unwrap_err(),
        "divide by zero"
    );
}

#[test]
fn substitution_cannot_capture_free_or_nested_bound_names() {
    let value = parse("y").unwrap();
    let term = parse("\\y -> \\y' -> x y").unwrap();
    let result = names::substitute(&term, "x", &value);
    assert_eq!(result.to_string(), "\\y'' -> \\y' -> y y''");
    assert_eq!(
        names::free(&result),
        std::collections::BTreeSet::from(["y".to_owned()])
    );
    assert_eq!(
        names::substitute(&parse("\\x -> x").unwrap(), "x", &value)
            .to_string(),
        "\\x -> x"
    );
    assert_eq!(
        names::substitute(&parse("let x = x in x").unwrap(), "x", &value)
            .to_string(),
        "let x = y in x"
    );
}

#[test]
fn debruijn_erases_alpha_names_but_preserves_free_names() {
    let a =
        names::debruijn(&parse("\\x -> \\y -> x").unwrap(), &mut vec![]);
    let b =
        names::debruijn(&parse("\\a -> \\b -> a").unwrap(), &mut vec![]);
    assert_eq!(a, b);
    assert_eq!(a, Term::Lam(Box::new(Term::Lam(Box::new(Term::Bound(1))))));
    assert_eq!(
        names::debruijn(&parse("x").unwrap(), &mut vec![]),
        Term::Free("x".into())
    );
}

#[test]
fn church_numerals_and_ski() {
    let church = "let two = \\f -> \\x -> f (f x) in let plus = \\m -> \\n -> \\f -> \\x -> m f (n f x) in plus two two (\\x -> x + 1) 0";
    assert_eq!(cbv(church, 1000).unwrap(), "4");
    assert_eq!(normal(church), "4");
    let ski = "let s = \\f -> \\g -> \\x -> f x (g x) in let k = \\x -> \\y -> x in let i = \\x -> x in s k k (i 42)";
    assert_eq!(cbv(ski, 1000).unwrap(), "42");
    assert_eq!(normal(ski), "42");
}

#[test]
fn y_combinator_terminates_only_under_normal_order() {
    let source = "let y = \\f -> (\\x -> f (x x)) (\\x -> f (x x)) in y (\\self -> \\n -> if n == 0 then 1 else n * self (n - 1)) 5";
    assert_eq!(normal(source), "120");
    assert_eq!(cbv(source, 200).unwrap_err(), "step limit exceeded");
    let omega = "(\\x -> x x) (\\x -> x x)";
    assert_eq!(
        normal::reduce(parse(omega).unwrap(), 20, |_| {}).unwrap_err(),
        "step limit exceeded"
    );
    assert_eq!(normal(&format!("(\\unused -> 42) ({omega})")), "42");
}

#[test]
fn printer_inserts_only_needed_parentheses() {
    for (source, expected) in [
        ("(((\\x -> x)))", "\\x -> x"),
        ("f (g x)", "f (g x)"),
        ("(f x) y", "f x y"),
        ("(\\x -> x) 3", "(\\x -> x) 3"),
        ("a - (b - c)", "a - (b - c)"),
        ("(a + b) * c", "(a + b) * c"),
        ("(a == b) == c", "(a == b) == c"),
        ("f (-x)", "f (-x)"),
    ] {
        let expr = parse(source).unwrap();
        assert_eq!(expr.to_string(), expected);
        let printed = parse(&expr.to_string()).unwrap();
        assert_eq!(
            names::debruijn(&expr, &mut vec![]),
            names::debruijn(&printed, &mut vec![])
        );
    }
}

#[test]
fn golden_corpus() {
    testkit::run_corpus(&testkit::Config {
        binary: std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch03")),
        programs: &std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/programs"),
        engines: &[testkit::Engine::Core],
        timeout: std::time::Duration::from_secs(10),
    })
    .unwrap();
}

#[test]
fn debruijn_snapshots() {
    for (name, source) in [
        ("identity", "\\x -> x"),
        ("constant", "\\x -> \\y -> x"),
        ("free", "\\x -> y"),
        ("apply", "\\f -> \\x -> f x"),
        ("shadow", "\\x -> \\x -> x"),
        ("let", "let x = 1 in x"),
        ("let_scope", "let x = x in x"),
        ("if", "\\b -> if b then 1 else 2"),
        ("arithmetic", "\\x -> x + 1"),
        ("two", "\\f -> \\x -> f (f x)"),
    ] {
        insta::assert_snapshot!(
            name,
            format!(
                "{:?}",
                names::debruijn(&parse(source).unwrap(), &mut vec![])
            )
        );
    }
}

#[test]
fn normal_reduction_preserves_open_arithmetic() {
    let expr = ch03_lambda::parse("\\x -> x + 1").unwrap();
    assert!(ch03_lambda::normal::step(&expr).unwrap().is_none());
}
