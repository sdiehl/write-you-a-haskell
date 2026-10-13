use ch02_calc::{eval, lexer, parse, pratt};

fn run(source: &str) -> i64 {
    let expression = parse(source).unwrap();
    eval::check(&expression).unwrap();
    eval::evaluate(&expression, &mut Vec::new()).unwrap()
}

#[test]
fn precedence_associativity_and_scope() {
    for (source, expected) in [
        ("1 + 2 * 3", 7),
        ("(1 + 2) * 3", 9),
        ("9 - 3 - 2", 4),
        ("24 / 3 / 2", 4),
        ("-2 * 3 + 8", 2),
        ("1--2", 3),
        ("let x = 5 in (let x = x + 1 in x) + x", 11),
        ("let x = let y = 4 in y in x * 2", 8),
        ("4611686018427387903 + 1", -4611686018427387904),
        ("(-4611686018427387904) / (-1)", -4611686018427387904),
    ] {
        assert_eq!(run(source), expected, "{source}");
    }
    assert_eq!(parse("1 + 2 * 3").unwrap().to_string(), "(+ 1 (* 2 3))");
}

#[test]
fn errors_have_precise_spans_and_recovery_continues() {
    let errors = parse("(1 + ) * (2 + )").unwrap_err();
    assert!(errors.len() >= 2, "{errors:?}");
    assert_eq!(errors[0].span, 5..6);
    assert!(errors.iter().any(|e| e.span == (14..15)));
    let errors = lexer::lex("1 + λ @").unwrap_err();
    assert_eq!(errors.len(), 2);
    assert_eq!(errors[0].span, 4..6);
    let expr = parse("let x = x in y").unwrap();
    assert_eq!(eval::check(&expr).unwrap_err().len(), 2);
    for source in [
        "",
        "1 2",
        "let x = 1 x",
        "(1",
        "1 +",
        "999999999999999999999999999",
    ] {
        assert!(parse(source).is_err(), "{source}");
        assert!(pratt::parse(source).is_err(), "{source}");
    }
}

#[test]
fn restoring_scope_on_failure_and_checking_without_evaluation() {
    let expr = parse("let x = 1 in 1 / 0").unwrap();
    assert!(eval::check(&expr).is_ok());
    let mut environment = vec![("outer".into(), 42)];
    assert_eq!(
        eval::evaluate(&expr, &mut environment),
        Err("divide by zero".into())
    );
    assert_eq!(environment, vec![("outer".into(), 42)]);
}

// Deterministic generation needs no random-number dependency and gives reproducible failures.
fn next(seed: &mut u64) -> u64 {
    *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
    *seed
}

fn generated(seed: &mut u64, depth: u8) -> String {
    if depth == 0 {
        return (next(seed) % 100).to_string();
    }
    match next(seed) % 6 {
        0 => format!(
            "(let x = {} in x + {})",
            generated(seed, depth - 1),
            generated(seed, depth - 1)
        ),
        1 => format!("-({})", generated(seed, depth - 1)),
        n => {
            let op = ["+", "-", "*", "/"][(n - 2) as usize];
            format!(
                "({} {op} {})",
                generated(seed, depth - 1),
                generated(seed, depth - 1)
            )
        }
    }
}

#[test]
fn two_parsers_agree_on_200_generated_expressions() {
    let mut seed = 20261010;
    for i in 0..200 {
        let source = generated(&mut seed, 1 + (i % 5));
        assert_eq!(
            parse(&source).unwrap(),
            pratt::parse(&source).unwrap(),
            "{source}"
        );
    }
    // Parentheses in generated terms are supplemented by unparenthesized chains.
    for source in [
        "1 + 2 * 3 - 4 / 2",
        "--3 + 2",
        "let x = 3 in x * 5 - 1",
        "1-2-3-4",
    ] {
        assert_eq!(parse(source).unwrap(), pratt::parse(source).unwrap());
    }
}

#[test]
fn golden_programs() {
    testkit::run_corpus(&testkit::Config {
        binary: std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch02")),
        programs: &std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/programs"),
        engines: &[testkit::Engine::Core],
        timeout: std::time::Duration::from_secs(10),
    })
    .unwrap();
}

#[test]
fn parse_dump_snapshots() {
    for (name, source) in [
        ("literal", "42"),
        ("precedence", "1 + 2 * 3"),
        ("associativity", "8 - 2 - 1"),
        ("parentheses", "(1 + 2) * 3"),
        ("negation", "--2"),
        ("let", "let x = 4 in x + x"),
        ("shadow", "let x = 1 in let x = x + 1 in x"),
        ("nested_rhs", "let x = let y = 4 in y in x"),
        ("division", "8 / 2 / 2"),
        ("mixed", "-(2 + 3) * 4 - 1"),
    ] {
        insta::assert_snapshot!(name, parse(source).unwrap().to_string());
    }
}
