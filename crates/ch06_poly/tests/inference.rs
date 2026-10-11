use ch06_poly::{algorithm_w, parse, types::Type, union_find};
fn agree(source: &str) -> Result<Type, &'static str> {
    let expr = parse(source).unwrap();
    let w = algorithm_w::check(&expr);
    let j = union_find::check(&expr);
    match (w, j) {
        (Ok(w), Ok(j)) => {
            assert_eq!(w, j, "{source}");
            Ok(j)
        }
        (Err(w), Err(j)) => {
            assert_eq!(w.code, j.code, "{source}");
            Err(j.code)
        }
        (w, j) => {
            panic!("inferencers disagree on {source}: W={w:?}; J={j:?}")
        }
    }
}
#[test]
fn principal_types_and_let_polymorphism() {
    for (source, expected) in [
        (r"\x -> x", "a -> a"),
        (r"\x -> \y -> x", "a -> b -> a"),
        (
            r"\f -> \g -> \x -> f (g x)",
            "(a -> b) -> (c -> a) -> c -> b",
        ),
        (r"let id = \x -> x in id id", "a -> a"),
        (r"let id = \x -> x in let a = id 1 in id True", "Bool"),
        (
            r"let id = (\f -> f) (\x -> x) in let a = id 1 in id True",
            "Bool",
        ),
        (r"\x -> let f = \y -> x in let a = f 1 in f True", "a -> a"),
        (r"let rec id = \x -> x in let a = id 1 in id True", "Bool"),
        (
            r"let rec f = \n -> if n == 0 then 0 else f (n - 1) in f",
            "Int -> Int",
        ),
    ] {
        assert_eq!(agree(source).unwrap().to_string(), expected);
    }
}
#[test]
fn captured_variables_are_never_generalized() {
    for source in [
        r"\x -> let y = x in let a = y 1 in y True",
        r"\x -> let y = \z -> x z in let a = y 1 in y True",
        r"\f -> let a = f 1 in f True",
        r"let rec f = \x -> if True then f 1 else f True in f 0",
        r"\x -> let f = \y -> x y in if f True then f 1 else False",
    ] {
        assert_eq!(agree(source), Err("E0201"));
    }
}
#[test]
fn cycles_fail_the_occurs_check() {
    for source in [
        r"\x -> x x",
        r"\f -> f (\x -> f)",
        r"let f = \x -> x x in f",
    ] {
        // The second term is also recursive in its inferred arrow structure.
        assert_eq!(agree(source), Err("E0202"));
    }
}
#[test]
fn both_algorithms_agree_on_the_entire_corpus() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/programs");
    for fixture in testkit::discover(&path).unwrap() {
        let source = std::fs::read_to_string(&fixture).unwrap();
        if parse(&source).is_ok() {
            let _ = agree(&source);
        }
    }
}
#[test]
fn generated_terms_agree_including_invalid_programs() {
    fn generate(seed: &mut u64, depth: usize, names: &[String]) -> String {
        *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        let choice = (*seed >> 32) as usize;
        if depth == 0 {
            return match choice % 3 {
                0 => "1".into(),
                1 => "True".into(),
                _ if !names.is_empty() => {
                    names[choice % names.len()].clone()
                }
                _ => "False".into(),
            };
        }
        let name = format!("x{}", names.len());
        let mut inner = names.to_vec();
        inner.push(name.clone());
        match choice % 6 {
            0 => format!(
                "(\\{name} -> {})",
                generate(seed, depth - 1, &inner)
            ),
            1 => format!(
                "({} {})",
                generate(seed, depth - 1, names),
                generate(seed, depth - 1, names)
            ),
            2 => format!(
                "(let {name} = {} in {})",
                generate(seed, depth - 1, names),
                generate(seed, depth - 1, &inner)
            ),
            3 => format!(
                "(if {} then {} else {})",
                generate(seed, depth - 1, names),
                generate(seed, depth - 1, names),
                generate(seed, depth - 1, names)
            ),
            4 => format!(
                "({} + {})",
                generate(seed, depth - 1, names),
                generate(seed, depth - 1, names)
            ),
            _ => generate(seed, 0, names),
        }
    }
    let mut seed = 2026;
    let (mut valid, mut invalid) = (0, 0);
    for _ in 0..1000 {
        let source = generate(&mut seed, 4, &[]);
        if agree(&source).is_ok() {
            valid += 1;
        } else {
            invalid += 1;
        }
    }
    assert!(valid > 100 && invalid > 100);
}
#[test]
fn golden_corpus() {
    testkit::run_corpus(&testkit::Config {
        binary: std::path::Path::new(env!("CARGO_BIN_EXE_fun-ch06")),
        programs: &std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/programs"),
        engines: &[testkit::Engine::Core, testkit::Engine::Cek],
        timeout: std::time::Duration::from_secs(10),
    })
    .unwrap();
}
#[test]
fn principal_type_snapshots() {
    for (name, source) in [
        ("identity", r"\x -> x"),
        ("constant", r"\x -> \y -> x"),
        ("apply", r"\f -> \x -> f x"),
        ("compose", r"\f -> \g -> \x -> f (g x)"),
        ("twice", r"\f -> \x -> f (f x)"),
        ("id_id", r"let id = \x -> x in id id"),
        ("let_poly", r"let id = \x -> x in let a = id 1 in id True"),
        ("captured", r"\x -> let f = \y -> x in f True"),
        (
            "recursive",
            r"let rec f = \n -> if n == 0 then 0 else f (n - 1) in f",
        ),
        ("fix", r"\f -> fix f"),
    ] {
        insta::assert_snapshot!(name, agree(source).unwrap().to_string());
    }
}
