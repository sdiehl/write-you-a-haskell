use ch14_classes::{
    core, elaborate, eval, parse, session::Session, typecheck,
};
fn compile(source: &str, prelude: bool) -> (String, core::Program) {
    let mut sess = Session::default();
    sess.options.no_prelude = !prelude;
    let file = sess.sources.add("classes.fun", source.into());
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let description = typed.main_description();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    core::lint(&core).unwrap();
    (description, core)
}
#[test]
fn dictionary_snapshots_are_typed_core() {
    let cases = [
        (
            "method",
            "class C a where { value :: a -> Int }\ninstance C Int where { value x = x }\nmain=value 42",
        ),
        (
            "default",
            "class C a where { value :: a -> Int; twice :: a -> Int; twice x=value x }\ninstance C Int where { value x=x }\nmain=twice 42",
        ),
        (
            "super",
            "class C a where { value :: a -> Int }\nclass C a => D a where { other :: a -> Int }\ninstance C Int where { value x=x }\ninstance D Int where { other x=x }\nf :: D a => a -> Int\nf x=value x\nmain=f 42",
        ),
        (
            "instance-context",
            "data Box a=Box a\nclass C a where { value :: a -> Int }\ninstance C Int where { value x=x }\ninstance C a => C (Box a) where { value (Box x)=value x }\nmain=value (Box 42)",
        ),
        (
            "constrained-value",
            "class C a where { value :: a -> Int }\ninstance C Int where { value x=x }\nf=value\nmain=f 42",
        ),
        (
            "constant",
            "class C a where { value :: a }\ninstance C Int where { value=42 }\nmain=value :: Int",
        ),
        (
            "polymorphic-field",
            "data Box a=Box a\nclass Run f where { run :: f a -> a }\ninstance Run Box where { run (Box x)=x }\nmain=run (Box 42)",
        ),
        (
            "polyrec",
            "data Nest a=End | Next a (Nest [a])\ndata [] a=[] | (:) a [a]\nsize :: Nest a -> Int\nsize End=42\nsize (Next x xs)=size xs\nmain=size (Next 0 End)",
        ),
        (
            "signature-components",
            "data (,) a b=(,) a b\ndata Bool=False|True\nfirst x y=x\nf x=first x g\ng :: Int -> (Int,Bool)\ng y=(f 1,f True)\nmain=42",
        ),
        (
            "super-chain",
            "class C a where { value :: a -> Int }\nclass C a => B a where {}\nclass B a => A a where {}\ninstance C Int where { value x=x }\ninstance B Int where {}\ninstance A Int where {}\nf :: A a => a -> Int\nf x=value x\nmain=f 42",
        ),
    ];
    for (name, source) in cases {
        let (_, program) = compile(source, false);
        assert_eq!(eval::run(&program, 100_000).unwrap(), "42");
        let declarations = program
            .constructors
            .iter()
            .filter(|c| c.id.name.starts_with("$MkD"))
            .map(|c| {
                format!(
                    "{} ({}) -> {}",
                    c.id.name,
                    c.fields
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", "),
                    c.result
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        let groups = program
            .groups
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(name, format!("{declarations}\n{groups}"));
    }
}
#[test]
fn qualified_types_simplify_superclasses_without_defaulting() {
    for (source, expected) in [
        ("main = (+)", "Num a => a -> a -> a"),
        (
            "main x y = if x < y then x == y else False",
            "Ord a => a -> a -> Bool",
        ),
        ("main x = show x", "Show a => a -> String"),
    ] {
        assert_eq!(compile(source, true).0, expected);
    }
}
#[test]
fn missing_dictionary_arguments_fail_independent_lint() {
    let (_, mut program) = compile(
        "class C a where { value :: a -> Int }\ninstance C Int where { value x=x }\nmain=value 42",
        false,
    );
    let id = program
        .groups
        .iter()
        .flat_map(|g| &g.binds)
        .find(|b| b.name.name.text == "value")
        .unwrap()
        .name
        .clone();
    let main = program
        .groups
        .iter_mut()
        .flat_map(|g| &mut g.binds)
        .find(|b| b.name.name.text == "main")
        .unwrap();
    *main.rhs = core::Expr::App(
        Box::new(core::Expr::TyApp(
            Box::new(core::Expr::Global(id)),
            core::Type::Int,
        )),
        Box::new(core::Expr::Lit(core::Lit::Int(42))),
    );
    assert!(core::lint(&program).unwrap_err().contains("type mismatch"));
}
#[test]
fn double_formats_and_round_trips() {
    use ch14_classes::primitives::show_double;
    for (n, expected) in [
        (0.0, "0.0"),
        (-0.0, "-0.0"),
        (5.0, "5.0"),
        (0.001, "0.001"),
        (0.0001, "0.0001"),
        (0.00001, "1.0e-5"),
        (1e16, "1.0e+16"),
        (2.5e-7, "2.5e-7"),
        (f64::INFINITY, "Infinity"),
        (f64::NEG_INFINITY, "-Infinity"),
        (f64::NAN, "NaN"),
        (f64::from_bits(1), "5.0e-324"),
    ] {
        assert_eq!(show_double(n), expected);
    }
    let mut bits = 17_u64;
    for _ in 0..200 {
        bits = bits.wrapping_mul(6364136223846793005).wrapping_add(1);
        let n = f64::from_bits(bits);
        if n.is_finite() {
            let text = show_double(n);
            assert_eq!(
                text.parse::<f64>().unwrap().to_bits(),
                bits,
                "{text}"
            );
        }
    }
}
#[test]
fn floating_literals_remain_monomorphic() {
    let (_, program) =
        compile("main = (1.0 + 2.0, 1 + 2, toDouble 1 + 2.0)", true);
    assert_eq!(eval::run(&program, 100_000).unwrap(), "(3.0,3,3.0)");
}

#[test]
fn user_file_names_cannot_expose_primitives() {
    let mut sess = Session::default();
    let file = sess.sources.add("<prelude>", "main=prim_intAdd 1 2".into());
    let ast = parse::parse(&mut sess, file).unwrap();
    assert!(typecheck::check(&mut sess, ast).is_err());
    assert!(sess.diagnostics.iter().any(|d| d.code
        == ch14_classes::diagnostics::ErrorCode::PrivatePrimitive));
}

#[test]
fn unary_minus_uses_the_prelude_method() {
    let (_, core) = compile("main = let negate x = 42 in -1", true);
    assert_eq!(eval::run(&core, 1_000_000).unwrap().to_string(), "-1");
}
