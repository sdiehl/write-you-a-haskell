use ch13_kinds::{
    core, elaborate, kinds::Kind, parse, session::Session, typecheck,
};

fn compile(source: &str) -> core::Program {
    let mut sess = Session::default();
    let file = sess.sources.add("kinds.fun", source.into());
    let ast = parse::parse(&mut sess, file).unwrap();
    let typed = typecheck::check(&mut sess, ast).unwrap();
    let core = elaborate::elaborate(&mut sess, typed).unwrap();
    core::lint(&core).unwrap();
    core
}

#[test]
fn kinded_core_snapshots() {
    for name in [
        "fix",
        "wrapper",
        "list-head",
        "partial-either",
        "arrow-head",
        "alias",
        "nested-alias",
        "alias-forward",
        "record",
        "hidden-witness",
    ] {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join(format!("tests/programs/kinds/{name}.fun"));
        let core = compile(&std::fs::read_to_string(path).unwrap());
        let kinds = core
            .data_types
            .iter()
            .filter(|(id, _)| !id.name.starts_with('$'))
            .map(|(id, k)| format!("{} :: {k}", id.name))
            .collect::<Vec<_>>()
            .join("\n");
        let groups = core
            .groups
            .iter()
            .skip(9)
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(name, format!("{kinds}\n{groups}"));
    }
}

#[test]
fn declaration_components_infer_before_defaulting() {
    let core = compile(
        "data A f = A (B f)\ndata B f = B (f Int)\ndata Phantom a = Phantom\nmain=1",
    );
    let lookup = |name| {
        core.data_types
            .iter()
            .find(|(id, _)| id.name == name)
            .unwrap()
            .1
            .clone()
    };
    let higher =
        Kind::arrow(Kind::arrow(Kind::Star, Kind::Star), Kind::Star);
    assert_eq!(lookup("A"), higher);
    assert_eq!(lookup("B"), higher);
    assert_eq!(lookup("Phantom"), Kind::arrow(Kind::Star, Kind::Star));
}

#[test]
fn lint_independently_rejects_wrong_parameter_kinds() {
    let mut core =
        compile("data Wrap f a = Wrap (f a)\nmain=Wrap (Just 1)");
    let con = core
        .constructors
        .iter()
        .find(|c| c.id.name == "Wrap")
        .unwrap();
    core.type_kinds.insert(con.parameters[0], Kind::Star);
    assert!(core::lint(&core).unwrap_err().contains("kind"));
}

#[test]
fn kind_occurs_check_terminates() {
    let mut store = ch13_kinds::kinds::Store::default();
    let k = store.fresh();
    let span =
        ch13_kinds::source::Span::new(ch13_kinds::source::FileId(0), 0, 1);
    assert!(
        store
            .unify(&k, &Kind::arrow(k.clone(), Kind::Star), span)
            .is_err()
    );
}

#[test]
fn synonym_dump_keeps_the_declaration() {
    let mut sess = Session::default();
    let file = sess
        .sources
        .add("alias.fun", "type Count = Int\nmain = 42".into());
    let program = parse::parse(&mut sess, file).unwrap();
    assert_eq!(program.aliases[0].to_string(), "type Count = Int");
}

#[test]
fn synonym_result_kind_can_be_a_constructor() {
    compile("type M = Maybe\nx :: M Int\nx = Just 42\nmain = x");
}
