// ANCHOR: rewrite-contract
//! A rewrite may delete an unused total RHS, replace an atom by an equal atom,
//! or move a lambda applied exactly once. It may never copy a call, a failing
//! primitive, or an effect, nor move one past another. Allocation failure is
//! outside the source cost model. The oracle checks transitions and trace order.
// ANCHOR_END: rewrite-contract
use crate::anf::{Anf, Atom, Body, Lambda, Program, Rhs, Var};
use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};
fn atom(a: &Atom, aliases: &BTreeMap<Var, Atom>) -> Atom {
    match a {
        Atom::Var(v) => {
            aliases.get(v).cloned().unwrap_or_else(|| a.clone())
        }
        _ => a.clone(),
    }
}
fn atoms(args: &[Atom], aliases: &BTreeMap<Var, Atom>) -> Vec<Atom> {
    args.iter().map(|a| atom(a, aliases)).collect()
}
fn mark(a: &Atom, free: &mut BTreeSet<Var>) {
    if let Atom::Var(v) = a {
        free.insert(v.clone());
    }
}
/// Unique binders let us collect uses and definitions in one iterative walk.
pub fn free_vars(body: &Body) -> BTreeSet<Var> {
    let mut used = BTreeSet::new();
    let mut bound = BTreeSet::new();
    let mut todo = vec![body.as_ref()];
    while let Some(b) = todo.pop() {
        match b {
            Anf::Ret(a) => mark(a, &mut used),
            Anf::TailCall(f, args) => {
                mark(f, &mut used);
                for a in args {
                    mark(a, &mut used);
                }
            }
            Anf::Jump(_, args) => {
                for a in args {
                    mark(a, &mut used);
                }
            }
            Anf::Fail(_) => {}
            Anf::Let(v, r, b) => {
                bound.insert(v.clone());
                todo.push(b);
                match r {
                    Rhs::Atom(a) | Rhs::Field(a, _) => mark(a, &mut used),
                    Rhs::Call(f, args) => {
                        mark(f, &mut used);
                        for a in args {
                            mark(a, &mut used);
                        }
                    }
                    Rhs::Con(_, args) | Rhs::Prim(_, args, _) => {
                        for a in args {
                            mark(a, &mut used);
                        }
                    }
                    Rhs::Lambda(l) => {
                        bound.extend(l.params.iter().cloned());
                        todo.push(&l.body);
                    }
                }
            }
            Anf::LetRec(bs, b) => {
                todo.push(b);
                for (v, l) in bs {
                    bound.insert(v.clone());
                    bound.extend(l.params.iter().cloned());
                    todo.push(&l.body);
                }
            }
            Anf::Join(_, ps, b, r) => {
                bound.extend(ps.iter().cloned());
                todo.push(b);
                todo.push(r);
            }
            Anf::Case(a, alts) => {
                mark(a, &mut used);
                for (_, ps, b) in alts {
                    bound.extend(ps.iter().cloned());
                    todo.push(b);
                }
            }
        }
    }
    used.retain(|v| !bound.contains(v));
    used
}
fn rhs_free(rhs: &Rhs, free: &mut BTreeSet<Var>) {
    match rhs {
        Rhs::Atom(a) | Rhs::Field(a, _) => mark(a, free),
        Rhs::Call(f, args) => {
            mark(f, free);
            for a in args {
                mark(a, free);
            }
        }
        Rhs::Con(_, args) | Rhs::Prim(_, args, _) => {
            for a in args {
                mark(a, free);
            }
        }
        Rhs::Lambda(l) => {
            let mut captures = free_vars(&l.body);
            for p in &l.params {
                captures.remove(p);
            }
            free.extend(captures);
        }
    }
}
/// Each result carries its free variables, avoiding a scan of every let suffix.
/// Binder identities are globally unique, so substitutions never need scope copies.
fn rewrite(sess: &mut crate::session::Session, body: &Body) -> Body {
    enum Work {
        Visit(Body),
        Let(Var, Rhs),
        Lambda(Var, Vec<Var>, Body),
        Rec(Vec<(Var, Lambda)>),
        Join(Var, Vec<Var>),
        Case(Atom, Vec<(crate::core::AltCon, Vec<Var>)>),
        Beta(Var, Body),
    }
    let mut todo = vec![Work::Visit(body.clone())];
    let mut out: Vec<(Body, BTreeSet<Var>)> = vec![];
    let mut aliases = BTreeMap::new();
    let mut known = BTreeMap::new();
    while let Some(work) = todo.pop() {
        match work {
            Work::Visit(b) => match b.as_ref() {
                Anf::Let(v, Rhs::Lambda(l), rest) => {
                    todo.push(Work::Lambda(
                        v.clone(),
                        l.params.clone(),
                        rest.clone(),
                    ));
                    todo.push(Work::Visit(l.body.clone()));
                }
                Anf::Let(v, r, rest) => {
                    let r = match r {
                        Rhs::Atom(a) => {
                            aliases.insert(v.clone(), atom(a, &aliases));
                            todo.push(Work::Visit(rest.clone()));
                            continue;
                        }
                        Rhs::Field(a, i) => {
                            Rhs::Field(atom(a, &aliases), *i)
                        }
                        Rhs::Call(f, args) => Rhs::Call(
                            atom(f, &aliases),
                            atoms(args, &aliases),
                        ),
                        Rhs::Con(c, args) => {
                            Rhs::Con(c.clone(), atoms(args, &aliases))
                        }
                        Rhs::Prim(p, args, s) => {
                            Rhs::Prim(*p, atoms(args, &aliases), *s)
                        }
                        Rhs::Lambda(_) => unreachable!(),
                    };
                    if let Rhs::Con(..) = r {
                        known.insert(v.clone(), r.clone());
                    }
                    todo.push(Work::Let(v.clone(), r));
                    todo.push(Work::Visit(rest.clone()));
                }
                Anf::LetRec(bs, rest) => {
                    todo.push(Work::Rec(bs.clone()));
                    todo.push(Work::Visit(rest.clone()));
                    todo.extend(
                        bs.iter()
                            .rev()
                            .map(|(_, l)| Work::Visit(l.body.clone())),
                    );
                }
                Anf::Join(j, ps, b, r) => {
                    todo.push(Work::Join(j.clone(), ps.clone()));
                    todo.push(Work::Visit(r.clone()));
                    todo.push(Work::Visit(b.clone()));
                }
                Anf::Case(a, alts) => {
                    let a = atom(a, &aliases);
                    if let Atom::Var(v) = &a && let Some(Rhs::Con(c, args)) = known.get(v)
                        && let Some((_, ps, b)) = alts.iter().find(|(h, _, _)| matches!(h, crate::core::AltCon::Default) || matches!(h, crate::core::AltCon::Con(d) if d == c)) {
                        for (p, a) in ps.iter().zip(args) { aliases.insert(p.clone(), a.clone()); }
                        todo.push(Work::Visit(b.clone()));
                    } else {
                        todo.push(Work::Case(a, alts.iter().map(|(c, ps, _)| (c.clone(), ps.clone())).collect()));
                        todo.extend(alts.iter().rev().map(|(_, _, b)| Work::Visit(b.clone())));
                    }
                }
                other => {
                    let b = Rc::new(match other {
                        Anf::Ret(a) => Anf::Ret(atom(a, &aliases)),
                        Anf::TailCall(f, args) => Anf::TailCall(
                            atom(f, &aliases),
                            atoms(args, &aliases),
                        ),
                        Anf::Jump(j, args) => {
                            Anf::Jump(j.clone(), atoms(args, &aliases))
                        }
                        Anf::Fail(s) => Anf::Fail(*s),
                        _ => unreachable!(),
                    });
                    let free = free_vars(&b);
                    out.push((b, free));
                }
            },
            Work::Lambda(v, params, rest) => {
                let (body, _) = out.pop().unwrap();
                todo.push(Work::Let(
                    v,
                    Rhs::Lambda(Lambda { params, body }),
                ));
                todo.push(Work::Visit(rest));
            }
            Work::Let(v, r) => {
                let (b, mut free) = out.pop().unwrap();
                if !free.remove(&v) && discardable(&r) {
                    out.push((b, free));
                    continue;
                }
                if let Rhs::Lambda(l) = &r {
                    let beta = match b.as_ref() {
                        Anf::TailCall(Atom::Var(f), args) if f == &v => {
                            Some((args, None))
                        }
                        Anf::Let(
                            result,
                            Rhs::Call(Atom::Var(f), args),
                            rest,
                        ) if f == &v && !free_vars(rest).contains(&v) => {
                            Some((args, Some((result, rest))))
                        }
                        _ => None,
                    };
                    if let Some((args, rest)) = beta
                        && args.len() == l.params.len()
                        && !args
                            .iter()
                            .any(|a| matches!(a, Atom::Var(w) if w == &v))
                    {
                        for (p, a) in l.params.iter().zip(args) {
                            aliases.insert(p.clone(), a.clone());
                        }
                        if let Some((result, rest)) = rest {
                            todo.push(Work::Beta(
                                result.clone(),
                                rest.clone(),
                            ));
                        }
                        todo.push(Work::Visit(l.body.clone()));
                        continue;
                    }
                }
                rhs_free(&r, &mut free);
                out.push((Rc::new(Anf::Let(v, r, b)), free));
            }
            Work::Rec(bs) => {
                let (rest, mut free) = out.pop().unwrap();
                let bodies = out.split_off(out.len() - bs.len());
                let bs: Vec<_> = bs
                    .into_iter()
                    .zip(bodies)
                    .map(|((v, l), (body, mut fs))| {
                        for p in &l.params {
                            fs.remove(p);
                        }
                        free.extend(fs);
                        (
                            v,
                            Lambda {
                                params: l.params,
                                body,
                            },
                        )
                    })
                    .collect();
                for (v, _) in &bs {
                    free.remove(v);
                }
                out.push((Rc::new(Anf::LetRec(bs, rest)), free));
            }
            Work::Join(j, ps) => {
                let (rest, rf) = out.pop().unwrap();
                let (body, mut free) = out.pop().unwrap();
                for p in &ps {
                    free.remove(p);
                }
                free.extend(rf);
                out.push((Rc::new(Anf::Join(j, ps, body, rest)), free));
            }
            Work::Case(a, heads) => {
                let bodies = out.split_off(out.len() - heads.len());
                let mut free = BTreeSet::new();
                mark(&a, &mut free);
                let alts = heads
                    .into_iter()
                    .zip(bodies)
                    .map(|((c, ps), (b, mut fs))| {
                        for p in &ps {
                            fs.remove(p);
                        }
                        free.extend(fs);
                        (c, ps, b)
                    })
                    .collect();
                out.push((Rc::new(Anf::Case(a, alts)), free));
            }
            Work::Beta(result, rest) => {
                let (entry, _) = out.pop().unwrap();
                let join = sess.fresh_id("$beta");
                let entry = plug(sess, &entry, &join);
                let body =
                    Rc::new(Anf::Join(join, vec![result], rest, entry));
                let free = free_vars(&body);
                out.push((body, free));
            }
        }
    }
    out.pop().unwrap().0
}
/// Calls and potentially failing/effectful primitives are never discarded.
pub fn simplify(sess: &mut crate::session::Session, program: &mut Program) {
    for (_, body) in &mut program.globals {
        *body = rewrite(sess, body);
    }
}

pub fn discardable(rhs: &Rhs) -> bool {
    use crate::{ast::Op, primitives::Prim};
    !matches!(
        rhs,
        Rhs::Call(..)
            | Rhs::Prim(Op::Div | Op::Mod, _, _)
            | Rhs::Prim(
                Op::Runtime(
                    Prim::Error
                        | Prim::Exit
                        | Prim::PutStr
                        | Prim::GetLine
                        | Prim::ChrChr
                        | Prim::IntDiv
                        | Prim::IntMod
                        | Prim::IntQuot
                        | Prim::IntRem
                ),
                _,
                _
            )
    )
}
// Replace returns with one shared continuation; lambda bodies keep their own returns.
fn plug(
    sess: &mut crate::session::Session,
    body: &Body,
    join: &Var,
) -> Body {
    let mut todo = vec![(body, false)];
    let mut out: Vec<Body> = vec![];
    while let Some((body, done)) = todo.pop() {
        if !done {
            todo.push((body, true));
            match body.as_ref() {
                Anf::Let(_, _, b) | Anf::LetRec(_, b) => {
                    todo.push((b, false))
                }
                Anf::Join(_, _, b, r) => {
                    todo.push((r, false));
                    todo.push((b, false));
                }
                Anf::Case(_, alts) => todo
                    .extend(alts.iter().rev().map(|(_, _, b)| (b, false))),
                _ => {}
            }
            continue;
        }
        let node = match body.as_ref() {
            Anf::Ret(a) => Anf::Jump(join.clone(), vec![a.clone()]),
            Anf::TailCall(f, args) => {
                let v = sess.fresh_id("$betaResult");
                Anf::Let(
                    v.clone(),
                    Rhs::Call(f.clone(), args.clone()),
                    Rc::new(Anf::Jump(join.clone(), vec![Atom::Var(v)])),
                )
            }
            Anf::Let(v, r, _) => {
                Anf::Let(v.clone(), r.clone(), out.pop().unwrap())
            }
            Anf::LetRec(bs, _) => {
                Anf::LetRec(bs.clone(), out.pop().unwrap())
            }
            Anf::Join(j, ps, _, _) => {
                let r = out.pop().unwrap();
                let b = out.pop().unwrap();
                Anf::Join(j.clone(), ps.clone(), b, r)
            }
            Anf::Case(a, alts) => {
                let bodies = out.split_off(out.len() - alts.len());
                Anf::Case(
                    a.clone(),
                    alts.iter()
                        .zip(bodies)
                        .map(|((c, ps, _), b)| (c.clone(), ps.clone(), b))
                        .collect(),
                )
            }
            other => other.clone(),
        };
        out.push(Rc::new(node));
    }
    out.pop().unwrap()
}
