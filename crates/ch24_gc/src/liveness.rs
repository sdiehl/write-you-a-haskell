//! Backward may-liveness on Flat control flow, including lexical join back-edges.
use crate::{
    flat::{Atom, Body, Flat, Rhs},
    names::Id,
};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

pub type Live = BTreeSet<Id>;

/// Pointer identities name immutable nodes only during one backend invocation.
pub fn key(node: &Flat) -> usize {
    std::ptr::from_ref(node) as usize
}

/// Live-in sets, named slot candidates, and a bound on per-operation scratch roots.
#[derive(Default)]
pub struct Liveness {
    pub before: BTreeMap<usize, Live>,
    pub binders: Live,
    pub scratch: usize,
}

struct Node {
    uses: Live,
    edges: Vec<(usize, Live)>,
}

fn atom(a: &Atom, uses: &mut Live) {
    if let Atom::Var(v) = a {
        uses.insert(v.clone());
    }
}

fn atoms(args: &[Atom], uses: &mut Live) {
    for a in args {
        atom(a, uses);
    }
}

/// Compute the least fixed point with a predecessor worklist, without recursive traversal.
pub fn analyze(body: &Body) -> Liveness {
    let mut result = Liveness::default();
    let mut nodes = BTreeMap::new();
    let mut joins = BTreeMap::new();
    let mut jumps = vec![];
    let mut pending = vec![body.as_ref()];
    while let Some(n) = pending.pop() {
        let id = key(n);
        let mut uses = Live::new();
        let mut edges = vec![];
        match n {
            Flat::Let(v, rhs, rest) => {
                result.binders.insert(v.clone());
                match rhs {
                    Rhs::Atom(a) | Rhs::Field(a, _) => atom(a, &mut uses),
                    Rhs::EnvField(v, _) => {
                        uses.insert(v.clone());
                    }
                    Rhs::CallKnown(_, e, args) => {
                        if let Some(e) = e {
                            atom(e, &mut uses);
                        }
                        atoms(args, &mut uses);
                        result.scratch = result.scratch.max(args.len() + 2);
                    }
                    Rhs::CallUnknown(f, args) => {
                        atom(f, &mut uses);
                        atoms(args, &mut uses);
                        result.scratch = result.scratch.max(args.len() + 2);
                    }
                    Rhs::AllocCon(_, args)
                    | Rhs::AllocClosure(_, args)
                    | Rhs::Prim(_, args, _) => {
                        atoms(args, &mut uses);
                        result.scratch = result.scratch.max(args.len() + 2);
                    }
                    Rhs::PatchClosure(a, _, b) => {
                        atom(a, &mut uses);
                        atom(b, &mut uses);
                    }
                }
                edges.push((key(rest), BTreeSet::from([v.clone()])));
                pending.push(rest);
            }
            Flat::Case(a, alts) => {
                atom(a, &mut uses);
                for (_, ps, b) in alts {
                    result.binders.extend(ps.iter().cloned());
                    edges.push((key(b), ps.iter().cloned().collect()));
                    pending.push(b);
                }
            }
            Flat::Join(j, ps, b, rest) => {
                result.binders.extend(ps.iter().cloned());
                joins.insert(
                    j.clone(),
                    (key(b), ps.iter().cloned().collect::<Live>()),
                );
                edges.push((key(rest), Live::new()));
                pending.extend([b.as_ref(), rest.as_ref()]);
            }
            Flat::Jump(j, args) => {
                atoms(args, &mut uses);
                jumps.push((id, j.clone()));
                result.scratch = result.scratch.max(args.len() + 2);
            }
            Flat::Ret(a) => atom(a, &mut uses),
            Flat::TailCallKnown(_, e, args) => {
                if let Some(e) = e {
                    atom(e, &mut uses);
                }
                atoms(args, &mut uses);
                result.scratch = result.scratch.max(args.len() + 2);
            }
            Flat::TailCallUnknown(f, args) => {
                atom(f, &mut uses);
                atoms(args, &mut uses);
                result.scratch = result.scratch.max(args.len() + 2);
            }
            Flat::Fail(_) => {}
        }
        result.binders.extend(uses.iter().cloned());
        nodes.insert(id, Node { uses, edges });
    }
    for (id, j) in jumps {
        nodes.get_mut(&id).unwrap().edges.push(joins[&j].clone());
    }
    let mut predecessors: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for (id, n) in &nodes {
        result.before.insert(*id, Live::new());
        for (next, _) in &n.edges {
            predecessors.entry(*next).or_default().push(*id);
        }
    }
    // ANCHOR: live-worklist
    let mut queue = nodes.keys().copied().collect::<VecDeque<_>>();
    let mut queued = nodes.keys().copied().collect::<BTreeSet<_>>();
    while let Some(id) = queue.pop_front() {
        queued.remove(&id);
        let n = &nodes[&id];
        let mut live = n.uses.clone();
        for (next, defined) in &n.edges {
            live.extend(result.before[next].difference(defined).cloned());
        }
        if result.before[&id] != live {
            result.before.insert(id, live);
            for p in predecessors.get(&id).into_iter().flatten() {
                if queued.insert(*p) {
                    queue.push_back(*p);
                }
            }
        }
    }
    // ANCHOR_END: live-worklist
    result
}
