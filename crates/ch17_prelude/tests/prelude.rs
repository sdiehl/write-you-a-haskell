use ch17_prelude::{
    core, derive, elaborate, eval, format, match_compile, parse,
    session::Session, typecheck,
};

#[test]
fn derived_syntax_snapshots_are_ordinary_checked_instances() {
    for (name, source) in [
        ("eq-enum", "data T = A | B deriving Eq"),
        ("eq-product", "data T a = T a Int deriving Eq"),
        ("ord-sum", "data T = A Int | B Char deriving (Eq, Ord)"),
        ("show-enum", "data T = A | B deriving Show"),
        ("show-product", "data T a = T a Int deriving Show"),
        (
            "show-record",
            "data T a = T {x :: a, y :: Int} deriving Show",
        ),
        (
            "recursive",
            "data T a = Leaf a | Fork (T a) (T a) deriving (Eq, Ord, Show)",
        ),
        ("phantom", "data T a = T deriving (Eq, Ord, Show)"),
        (
            "mutual",
            "data A = A B deriving (Eq, Show)\ndata B = B (Maybe A) deriving (Eq, Show)",
        ),
        ("nested", "data T a = T [Maybe a] deriving Show"),
    ] {
        let mut sess = Session::default();
        let file = sess
            .sources
            .add("derive.fun", format!("{source}\nmain = ()"));
        let ast = parse::parse(&mut sess, file).unwrap();
        let mut expanded = ast.clone();
        derive::derive(&mut sess, &mut expanded).unwrap();
        insta::assert_snapshot!(
            name,
            expanded
                .instances
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n")
        );
        let typed = typecheck::check(&mut sess, ast)
            .unwrap_or_else(|_| panic!("{source}: {:?}", sess.diagnostics));
        let core = elaborate::elaborate(&mut sess, typed).unwrap();
        core::lint(&core).unwrap();
        let core = match_compile::compile(&mut sess, core).unwrap();
        core::lint(&core).unwrap();
        assert_eq!(eval::run(&core, 1_000_000).unwrap(), "()");
    }
}
#[test]
fn deriving_and_ranges_format_idempotently() {
    let mut sess = Session::default();
    let file=sess.sources.add("format.fun","data T = T Int deriving (Eq, Ord, Show)\n-- inclusive\nmain = map T [1..3]\n".into());
    let once = format::format(&mut sess, file).unwrap();
    let file = sess.sources.add("formatted.fun", once.clone());
    assert_eq!(format::format(&mut sess, file).unwrap(), once);
}
