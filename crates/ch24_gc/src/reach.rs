//! Preserve ordered initialization while retaining a witness for each reachable global.
use crate::{
    anf::{Anf, Atom, Body, Program, Rhs},
    names::Id,
};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// Source-ordered survivors and their first discovery edge. Roots have no predecessor.
pub struct Reach {
    pub kept: Vec<Id>,
    pub witness: BTreeMap<Id, Option<Id>>,
}
/// Visit every reference, including captures hidden in nested lambdas, without recursion.
fn references(body: &Body) -> BTreeSet<Id> {
    let mut found = BTreeSet::new();
    let mut todo = vec![body.as_ref()];
    let mut mark = |a: &Atom| {
        if let Atom::Global(v) = a {
            found.insert(v.clone());
        }
    };
    while let Some(body) = todo.pop() {
        match body {
            Anf::Let(_, rhs, rest) => {
                todo.push(rest);
                match rhs {
                    Rhs::Atom(a) | Rhs::Field(a, _) => mark(a),
                    Rhs::Call(f, args) => {
                        mark(f);
                        for a in args {
                            mark(a);
                        }
                    }
                    Rhs::Con(_, args) | Rhs::Prim(_, args, _) => {
                        for a in args {
                            mark(a);
                        }
                    }
                    Rhs::Lambda(l) => todo.push(&l.body),
                }
            }
            Anf::LetRec(bs, rest) => {
                todo.push(rest);
                for (_, l) in bs {
                    todo.push(&l.body);
                }
            }
            Anf::Join(_, _, body, rest) => {
                todo.push(rest);
                todo.push(body);
            }
            Anf::Case(a, alts) => {
                mark(a);
                for (_, _, body) in alts {
                    todo.push(body);
                }
            }
            Anf::Ret(a) => mark(a),
            Anf::TailCall(f, args) => {
                mark(f);
                for a in args {
                    mark(a);
                }
            }
            Anf::Jump(_, args) => {
                for a in args {
                    mark(a);
                }
            }
            Anf::Fail(_) => {}
        }
    }
    found
}
/// A closure's body does not run during initialization. Calls and failure stay roots.
pub fn discardable(body: &Body) -> bool {
    let mut todo = vec![body.as_ref()];
    while let Some(body) = todo.pop() {
        match body {
            Anf::Let(_, rhs, rest) => {
                if !crate::anf_simplify::discardable(rhs) {
                    return false;
                }
                todo.push(rest);
            }
            Anf::LetRec(_, rest) => todo.push(rest),
            Anf::Ret(_) => {}
            // Control-flow initializers are conservatively retained. A jump may loop.
            _ => return false,
        }
    }
    true
}
/// Follow dependencies from explicit roots and observable initializers in stable order.
pub fn analyze(program: &Program, roots: &[Id]) -> Reach {
    let bodies = program
        .globals
        .iter()
        .map(|(id, body)| (id.clone(), body))
        .collect::<BTreeMap<_, _>>();
    let mut witness = BTreeMap::new();
    let mut todo = VecDeque::new();
    for id in roots.iter().cloned().chain(
        program
            .globals
            .iter()
            .filter(|(_, b)| !discardable(b))
            .map(|(v, _)| v.clone()),
    ) {
        if bodies.contains_key(&id) && !witness.contains_key(&id) {
            witness.insert(id.clone(), None);
            todo.push_back(id);
        }
    }
    while let Some(id) = todo.pop_front() {
        for next in references(bodies[&id]) {
            if bodies.contains_key(&next) && !witness.contains_key(&next) {
                witness.insert(next.clone(), Some(id.clone()));
                todo.push_back(next);
            }
        }
    }
    Reach {
        kept: program
            .globals
            .iter()
            .filter(|(v, _)| witness.contains_key(v))
            .map(|(v, _)| v.clone())
            .collect(),
        witness,
    }
}
/// Filtering preserves the original dependency and initialization order.
pub fn prune(program: &mut Program, reach: &Reach) {
    program
        .globals
        .retain(|(id, _)| reach.witness.contains_key(id));
}
/// Stable diagnostic spelling keeps unique-number churn out of the witness dump.
pub fn display(reach: &Reach, original: usize) -> String {
    use std::fmt::Write;
    let mut text = String::new();
    for id in &reach.kept {
        let parent = reach.witness[id]
            .as_ref()
            .map_or("root", |p| p.text.as_str());
        writeln!(text, "{} <- {parent}", id.text).unwrap();
    }
    writeln!(
        text,
        "kept {}, dropped {}",
        reach.kept.len(),
        original - reach.kept.len()
    )
    .unwrap();
    text
}
