//! Iterative IR printing, with type evidence only on value binders.
use crate::{
    anf::{Anf, Rhs, Var},
    flat::Flat,
    rep::{Rep, Table},
};

/// A word-represented type needs no annotation in the teaching dumps.
pub fn binder(v: &Var, reps: &Table) -> String {
    match reps.get(v).expect("dump binder evidence") {
        Rep::Word => v.to_string(),
        rep => format!("{v} : {rep}"),
    }
}

pub fn binders(vs: &[Var], reps: &Table) -> String {
    vs.iter()
        .map(|v| binder(v, reps))
        .collect::<Vec<_>>()
        .join(", ")
}

fn atoms(vs: &[crate::anf::Atom]) -> String {
    vs.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

enum Visit<'a> {
    Anf(&'a Anf),
    Flat(&'a Flat),
    Text(String),
}

/// Print ANF without using the host stack for nested lambdas or control flow.
pub fn anf(body: &Anf, reps: &Table) -> String {
    render(Visit::Anf(body), reps)
}

/// Print Flat IR with the same binder notation as ANF.
pub fn flat(body: &Flat, reps: &Table) -> String {
    render(Visit::Flat(body), reps)
}

fn render(first: Visit<'_>, reps: &Table) -> String {
    let mut out = String::new();
    let mut work = vec![first];
    while let Some(item) = work.pop() {
        match item {
            Visit::Text(s) => out.push_str(&s),
            Visit::Anf(body) => match body {
                Anf::Let(v, Rhs::Lambda(l), rest) => {
                    out.push_str(&format!(
                        "let {} = \\({}) -> {{ ",
                        binder(v, reps),
                        binders(&l.params, reps)
                    ));
                    work.extend([
                        Visit::Anf(rest),
                        Visit::Text(" };\n".into()),
                        Visit::Anf(&l.body),
                    ]);
                }
                Anf::Let(v, r, rest) => {
                    out.push_str(&format!(
                        "let {} = {r};\n",
                        binder(v, reps)
                    ));
                    work.push(Visit::Anf(rest));
                }
                Anf::LetRec(bs, rest) => {
                    work.push(Visit::Anf(rest));
                    for (v, l) in bs.iter().rev() {
                        work.extend([
                            Visit::Text(" }\n".into()),
                            Visit::Anf(&l.body),
                            Visit::Text(format!(
                                "rec {}({}) = {{ ",
                                binder(v, reps),
                                binders(&l.params, reps)
                            )),
                        ]);
                    }
                }
                Anf::Join(j, ps, b, r) => {
                    out.push_str(&format!(
                        "join {j}({}) = {{ ",
                        binders(ps, reps)
                    ));
                    work.extend([
                        Visit::Anf(r),
                        Visit::Text(" };\n".into()),
                        Visit::Anf(b),
                    ]);
                }
                Anf::Case(a, alts) => {
                    out.push_str(&format!("case {a} {{\n"));
                    work.push(Visit::Text("}".into()));
                    for (c, ps, b) in alts.iter().rev() {
                        work.extend([
                            Visit::Text(" }\n".into()),
                            Visit::Anf(b),
                            Visit::Text(format!(
                                "{}({}) -> {{ ",
                                alternative(c),
                                binders(ps, reps)
                            )),
                        ]);
                    }
                }
                Anf::Jump(j, args) => {
                    out.push_str(&format!("jump {j}({})", atoms(args)))
                }
                Anf::Ret(a) => out.push_str(&format!("return {a}")),
                Anf::TailCall(f, args) => {
                    out.push_str(&format!("tail {f}({})", atoms(args)))
                }
                Anf::Fail(s) => out.push_str(&format!("fail {s:?}")),
            },
            Visit::Flat(body) => match body {
                Flat::Let(v, r, rest) => {
                    out.push_str(&format!(
                        "let {} = {r};\n",
                        binder(v, reps)
                    ));
                    work.push(Visit::Flat(rest));
                }
                Flat::Join(j, ps, b, r) => {
                    out.push_str(&format!(
                        "join {j}({}) = {{ ",
                        binders(ps, reps)
                    ));
                    work.extend([
                        Visit::Flat(r),
                        Visit::Text(" };\n".into()),
                        Visit::Flat(b),
                    ]);
                }
                Flat::Case(a, alts) => {
                    out.push_str(&format!("case {a} {{\n"));
                    work.push(Visit::Text("}".into()));
                    for (c, ps, b) in alts.iter().rev() {
                        work.extend([
                            Visit::Text(" }\n".into()),
                            Visit::Flat(b),
                            Visit::Text(format!(
                                "{}({}) -> {{ ",
                                alternative(c),
                                binders(ps, reps)
                            )),
                        ]);
                    }
                }
                // These variants contain no nested IR, so Display cannot recurse.
                leaf => out.push_str(&leaf.to_string()),
            },
        }
    }
    out
}

fn alternative(c: &crate::core::AltCon) -> String {
    match c {
        crate::core::AltCon::Con(c) => c.name.clone(),
        crate::core::AltCon::Lit(l) => l.to_string(),
        crate::core::AltCon::Default => "_".into(),
    }
}
