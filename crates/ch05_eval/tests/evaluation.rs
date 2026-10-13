use ch05_eval::{cbn, cek, eval, parse, types};

fn checked(source: &str) -> ch05_eval::ast::Expr {
    let expr = parse(source).unwrap();
    types::check(&expr).unwrap();
    expr
}

#[test]
fn a_million_tail_calls_use_bounded_continuations() {
    let source = r"let rec go : Int -> Int -> Int = \(acc : Int) -> \(n : Int) -> if n == 0 then acc else go (acc + 1) (n - 1) in go 0 1000000";
    // The worker's small host stack makes accidental evaluator recursion visible.
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(move || {
            let result =
                cek::evaluate(&checked(source), 100_000_000).unwrap();
            assert_eq!(result.value.to_string(), "1000000");
            assert!(
                result.max_continuations <= 8,
                "{} frames",
                result.max_continuations
            );
            let short = source.replace("1000000", "100");
            let short = cek::evaluate(&checked(&short), 100_000).unwrap();
            assert_eq!(result.max_continuations, short.max_continuations);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn non_tail_work_is_retained_on_the_explicit_stack() {
    let expr = checked(
        r"let rec sum : Int -> Int = \(n : Int) -> if n == 0 then 0 else n + sum (n - 1) in sum 1000",
    );
    let result = cek::evaluate(&expr, 100_000).unwrap();
    assert_eq!(result.value.to_string(), "500500");
    assert!(result.max_continuations >= 1000);
}

#[test]
fn call_by_name_discards_unused_errors_and_does_not_enter_lambdas() {
    for source in [r"(\(x : Int) -> 42) (1 / 0)", "let x = 1 / 0 in 42"] {
        let expr = checked(source);
        assert_eq!(
            cbn::evaluate(&expr, &mut 1000).unwrap().to_string(),
            "42"
        );
        assert_eq!(
            cek::evaluate(&expr, 1000).err().unwrap(),
            "divide by zero"
        );
        assert_eq!(
            eval::evaluate(&expr, &vec![], &mut 1000).unwrap_err(),
            "divide by zero"
        );
    }
    let expr = checked(r"\(x : Int) -> 1 / 0");
    assert!(cbn::evaluate(&expr, &mut 100).is_ok());
}

#[test]
fn fixed_points_unfold_and_fuel_bounds_divergence() {
    let expr = checked(r"fix (\(x : Int) -> x)");
    assert_eq!(
        cek::evaluate(&expr, 100).err().unwrap(),
        "step limit exceeded"
    );
    assert_eq!(
        eval::evaluate(&expr, &vec![], &mut 100).unwrap_err(),
        "step limit exceeded"
    );
    assert_eq!(
        cbn::evaluate(&expr, &mut 100).unwrap_err(),
        "step limit exceeded"
    );
    let expr = checked(
        r"fix (\(f : Int -> Int) -> \(n : Int) -> if n == 0 then 1 else n * f (n - 1)) 6",
    );
    assert_eq!(
        cbn::evaluate(&expr, &mut 10_000).unwrap().to_string(),
        "720"
    );
    assert_eq!(
        cek::evaluate(&expr, 10_000).unwrap().value.to_string(),
        "720"
    );
}

#[test]
fn left_to_right_order_is_observable_before_divergence() {
    let expr = checked(r"(1 / 0) + fix (\(x : Int) -> x)");
    assert_eq!(cek::evaluate(&expr, 100).err().unwrap(), "divide by zero");
    let expr = checked(r"fix (\(x : Int) -> x) + (1 / 0)");
    assert_eq!(
        cek::evaluate(&expr, 100).err().unwrap(),
        "step limit exceeded"
    );
}

#[test]
fn normal_reduction_preserves_open_arithmetic() {
    let expr = ch05_eval::parse("\\(x : Int) -> x + 1").unwrap();
    assert!(ch05_eval::normal::step(&expr).unwrap().is_none());
}
