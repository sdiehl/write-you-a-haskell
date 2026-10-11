use ch04_stlc::{
    eval, names, normal, parse,
    types::{self, Type},
};
fn ty(source: &str) -> Type {
    types::check(&parse(source).unwrap()).unwrap()
}
#[test]
fn higher_order_types_and_shadowing() {
    assert_eq!(
        ty(r"\(f : Int -> Bool) -> \(x : Int) -> f x").to_string(),
        "(Int -> Bool) -> Int -> Bool"
    );
    assert_eq!(ty(r"let x = True in let x = 3 in x"), Type::Int);
    assert_eq!(
        ty(r"\(f : (Int -> Bool) -> Int) -> f").to_string(),
        "((Int -> Bool) -> Int) -> (Int -> Bool) -> Int"
    );
}
#[test]
fn errors_point_to_the_expression_with_the_wrong_type() {
    for (source, span, message) in [
        (
            "if 1 then True else False",
            3..4,
            "expected Bool, found Int",
        ),
        (
            "if True then 1 else False",
            20..25,
            "expected Int, found Bool",
        ),
        ("1 True", 0..1, "expected a function type, found Int"),
        (
            r"(\(x : Int) -> x) True",
            18..22,
            "expected Int, found Bool",
        ),
    ] {
        let error = types::check(&parse(source).unwrap()).unwrap_err();
        assert_eq!(error.code, "E0201");
        assert_eq!(error.span, span);
        assert_eq!(error.message, message);
    }
}
#[test]
fn lambdas_need_annotations_and_let_does_not_generalize() {
    assert!(parse(r"\x -> x").is_err());
    let expr =
        parse(r"let id = \(x : Int) -> x in let a = id 1 in id True")
            .unwrap();
    assert_eq!(types::check(&expr).unwrap_err().code, "E0201");
    assert!(types::check(&parse(r"\(x : Int) -> x x").unwrap()).is_err());
}
#[test]
fn reduction_preserves_types_and_annotations() {
    for source in [
        r"(\(x : Int) -> x + 1) 3",
        r"(\(f : Int -> Int) -> f 2) (\(n : Int) -> n * 4)",
        r"let id = \(x : Bool) -> x in id True",
        r"if 1 < 2 then 3 else 4",
        r"(\(x : Int) -> \(y : Bool) -> x) 3",
    ] {
        let mut expr = parse(source).unwrap();
        let expected = types::check(&expr).unwrap();
        while let Some(next) = normal::step(&expr).unwrap() {
            assert_eq!(types::check(&next).unwrap(), expected);
            expr = next;
        }
        let printed = parse(&expr.to_string()).unwrap();
        assert_eq!(
            names::debruijn(&expr, &mut vec![]),
            names::debruijn(&printed, &mut vec![])
        );
    }
}
#[test]
fn checked_programs_reuse_strict_closures() {
    let expr = parse(
        r"let x = 1 in let f = \(y : Int) -> x + y in let x = 100 in f 2",
    )
    .unwrap();
    assert_eq!(types::check(&expr).unwrap(), Type::Int);
    assert_eq!(
        eval::evaluate(&expr, &vec![], &mut 1000)
            .unwrap()
            .to_string(),
        "3"
    );
    let expr = parse(r"(\(x : Int) -> 1) (1 / 0)").unwrap();
    assert_eq!(types::check(&expr).unwrap(), Type::Int);
    assert_eq!(
        eval::evaluate(&expr, &vec![], &mut 1000).unwrap_err(),
        "divide by zero"
    );
}
#[test]
fn golden_corpus() {
    testkit::run_corpus(&testkit::Config {
        binary: std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch04")),
        programs: &std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/programs"),
        engines: &[testkit::Engine::Core],
        timeout: std::time::Duration::from_secs(10),
    })
    .unwrap();
}
#[test]
fn type_snapshots() {
    for (name, source) in [
        ("integer", "42"),
        ("boolean", "True"),
        ("identity", r"\(x : Int) -> x"),
        ("constant", r"\(x : Int) -> \(y : Bool) -> x"),
        ("apply", r"\(f : Int -> Bool) -> \(x : Int) -> f x"),
        ("two", r"\(f : Int -> Int) -> \(x : Int) -> f (f x)"),
        ("predicate", r"\(x : Int) -> x <= 0"),
        ("choose", r"\(b : Bool) -> if b then 1 else 2"),
        ("let", r"let id = \(x : Int) -> x in id 1"),
        ("shadow", r"\(x : Int) -> \(x : Bool) -> x"),
    ] {
        insta::assert_snapshot!(name, ty(source).to_string());
    }
}
