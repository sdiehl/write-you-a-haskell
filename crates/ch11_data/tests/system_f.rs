use ch11_data::{
    core::{self, Bind, BindGroup, Expr, Id, Lit, Program, Type},
    elaborate, eval, parse,
    session::Session,
    typecheck,
};
fn program(rhs: Expr, ty: Type) -> Program {
    let mut sess = Session::default();
    let main = Id {
        name: sess.fresh_id("main"),
        ty,
    };
    Program {
        constructors: vec![],
        data_types: vec![],
        locations: vec![],
        groups: vec![BindGroup {
            recursive: false,
            binds: vec![Bind {
                name: main.clone(),
                rhs: Box::new(rhs),
            }],
        }],
        main,
    }
}
fn identity(variable: u32, unique: u32) -> Expr {
    let id = Id {
        name: ch11_data::names::Id {
            name: ch11_data::names::Name {
                sym: ch11_data::names::Symbol(1),
                unique,
            },
            text: "x".into(),
        },
        ty: Type::Var(variable),
    };
    Expr::TyLam(
        variable,
        Box::new(Expr::Lam(id.clone(), Box::new(Expr::Var(id)))),
    )
}
fn identity_type(variable: u32) -> Type {
    Type::Forall(
        variable,
        Box::new(Type::arrow(Type::Var(variable), Type::Var(variable))),
    )
}
#[test]
fn substitution_avoids_capture_and_types_compare_up_to_alpha() {
    let ty =
        Type::Forall(1, Box::new(Type::arrow(Type::Var(0), Type::Var(1))));
    let substituted = ty.substitute(0, &Type::Var(1));
    assert_eq!(substituted.free(), std::collections::BTreeSet::from([1]));
    assert!(substituted.equivalent(&Type::Forall(
        99,
        Box::new(Type::arrow(Type::Var(1), Type::Var(99)))
    )));
    assert!(!identity_type(1).equivalent(&Type::Forall(
        2,
        Box::new(Type::arrow(Type::Var(1), Type::Var(2)))
    )));
}
#[test]
fn lint_accepts_system_f_including_polymorphic_type_arguments() {
    core::lint(&program(identity(0, 1), identity_type(12))).unwrap();
    let rhs = Expr::App(
        Box::new(Expr::TyApp(Box::new(identity(0, 1)), identity_type(2))),
        Box::new(identity(3, 2)),
    );
    core::lint(&program(rhs, identity_type(4))).unwrap();
}
#[test]
fn lint_rejects_malformed_core_without_surface_inference() {
    let int = || Expr::Lit(Lit::Int(1));
    let type_id = || Expr::TyApp(Box::new(identity(0, 1)), Type::Int);
    let span =
        ch11_data::source::Span::new(ch11_data::source::FileId(0), 0, 0);
    for (rhs, ty, fragment) in [
        (
            Expr::App(Box::new(identity(0, 1)), Box::new(int())),
            Type::Int,
            "non-function",
        ),
        (
            Expr::App(
                Box::new(type_id()),
                Box::new(Expr::Lit(Lit::String("x".into()))),
            ),
            Type::Int,
            "type mismatch",
        ),
        (
            Expr::TyApp(Box::new(int()), Type::Int),
            Type::Int,
            "non-polymorphic",
        ),
        (
            Expr::TyApp(Box::new(identity(0, 1)), Type::Var(77)),
            Type::Int,
            "unbound Core type variable",
        ),
        (
            Expr::Prim(ch11_data::ast::Op::Add, vec![], vec![int()], span),
            Type::Int,
            "unsaturated",
        ),
        (
            Expr::Prim(
                ch11_data::ast::Op::Add,
                vec![Type::Int],
                vec![int(), int()],
                span,
            ),
            Type::Int,
            "unsaturated",
        ),
        (
            Expr::PatternFail(span, Type::String),
            Type::Int,
            "type mismatch",
        ),
        (int(), Type::Bool, "type mismatch"),
        (
            Expr::TyLam(0, Box::new(identity(0, 1))),
            Type::Forall(0, Box::new(identity_type(0))),
            "duplicate Core type binder",
        ),
    ] {
        let error = core::lint(&program(rhs, ty)).unwrap_err();
        assert!(error.contains(fragment), "{error}");
    }
    let mut malformed = program(int(), Type::Int);
    malformed.groups.push(malformed.groups[0].clone());
    assert!(
        core::lint(&malformed)
            .unwrap_err()
            .contains("duplicate Core binder")
    );
}
#[test]
fn generated_hm_programs_elaborate_to_well_typed_core() {
    for n in 0..200 {
        let expr = match n % 5 {
            0 => format!("id id {n}"),
            1 => format!(
                "let {{ f x = let {{ g y = x; }} in g True; }} in f {n}"
            ),
            2 => format!("(\\f -> {n}) (\\x -> x)"),
            3 => format!(
                "let {{ f k x = if k == 0 then x else g (k-1) x; g k x = f k x; }} in f 2 {n}"
            ),
            _ => format!("let {{ id :: Int -> Int; id x = x; }} in id {n}"),
        };
        let mut sess = Session::default();
        let file = sess
            .sources
            .add("generated.fun", format!("id x = x\nmain = {expr}"));
        let ast = parse::parse(&mut sess, file).unwrap();
        let typed = typecheck::check(&mut sess, ast).unwrap();
        let core = elaborate::elaborate(&mut sess, typed).unwrap();
        core::lint(&core).unwrap_or_else(|error| panic!("{expr}: {error}"));
        assert_eq!(eval::run(&core, 10000).unwrap(), n.to_string());
    }
}
