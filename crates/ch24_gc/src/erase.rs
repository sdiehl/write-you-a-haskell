//! Remove System F syntax after recording each binder and expression's type evidence.
use crate::{
    ast::Op, core, data::ConId, names::Id, rep::Rep, source::Span,
};
use std::collections::BTreeMap;
#[derive(Clone, Debug)]
/// Expression-result evidence supplies the type of fresh ANF temporaries.
pub struct Expr {
    pub node: Node,
    pub rep: Rep,
}
#[derive(Clone, Debug)]
/// Untyped Core retains strict evaluation order and lexical join points.
pub enum Node {
    Var(Id),
    Global(Id),
    Lit(core::Lit),
    Lam(Id, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(bool, Vec<(Id, Expr)>, Box<Expr>),
    Con(ConId, Vec<Expr>),
    Prim(Op, Vec<Expr>, Span),
    Case(Box<Expr>, Id, Vec<(core::AltCon, Vec<Id>, Expr)>),
    Join(Id, Vec<Id>, Box<Expr>, Box<Expr>),
    Jump(Id, Vec<Expr>),
    Fail(Span),
}
#[derive(Clone, Debug)]
pub struct Program {
    pub globals: Vec<(Id, Expr)>,
    pub reps: BTreeMap<Id, Rep>,
    pub metadata: core::Program,
}
/// This is the single classification point. A type variable proves no numeric representation.
pub(crate) fn representation(ty: &core::Type) -> Rep {
    match ty {
        core::Type::Int => Rep::Int,
        core::Type::Double => Rep::Double,
        _ => Rep::Word,
    }
}
/// Postorder erasure uses a worklist so expression depth does not consume host stack.
fn expression(
    root: &core::Expr,
    program: &core::Program,
    reps: &mut BTreeMap<Id, Rep>,
) -> Expr {
    use core::{Expr as C, Type as T};
    let mut todo = vec![(root, false)];
    let mut results: Vec<(Expr, T)> = vec![];
    while let Some((e, done)) = todo.pop() {
        let children: Vec<&core::Expr> = match e {
            C::Lam(_, e) | C::TyLam(_, e) | C::TyApp(e, _) => vec![e],
            C::App(f, a) => vec![f, a],
            C::Let(g, b) => g
                .binds
                .iter()
                .map(|b| b.rhs.as_ref())
                .chain(std::iter::once(b.as_ref()))
                .collect(),
            C::Con(_, _, es) | C::Prim(_, _, es, _) | C::Jump(_, es, _) => {
                es.iter().collect()
            }
            C::Case(e, _, _, alts) => std::iter::once(e.as_ref())
                .chain(alts.iter().map(|a| &a.rhs))
                .collect(),
            C::Join(_, _, b, r) => vec![b, r],
            C::Match(_) => panic!("match compilation precedes erasure"),
            _ => vec![],
        };
        if !done {
            todo.push((e, true));
            todo.extend(children.into_iter().rev().map(|c| (c, false)));
            continue;
        }
        let start = results.len() - children.len();
        let mut parts = results.split_off(start).into_iter();
        let mut bind = |i: &core::Id| {
            reps.insert(i.name.clone(), representation(&i.ty));
            i.name.clone()
        };
        let (node, ty) = match e {
            C::Var(i) => (Node::Var(i.name.clone()), i.ty.clone()),
            C::Global(i) => (Node::Global(i.name.clone()), i.ty.clone()),
            C::Lit(l) => (
                Node::Lit(l.clone()),
                match l {
                    core::Lit::Int(_) => T::Int,
                    core::Lit::Double(_) => T::Double,
                    core::Lit::Char(_) => T::Char,
                    core::Lit::String(_) => T::String,
                },
            ),
            C::Lam(i, _) => {
                let (b, t) = parts.next().unwrap();
                (Node::Lam(bind(i), Box::new(b)), T::arrow(i.ty.clone(), t))
            }
            C::App(_, _) => {
                let (f, t) = parts.next().unwrap();
                let (a, _) = parts.next().unwrap();
                let T::Arrow(_, t) = t else {
                    panic!("checked Core application")
                };
                (Node::App(Box::new(f), Box::new(a)), *t)
            }
            C::TyLam(v, _) => {
                let (e, t) = parts.next().unwrap();
                (e.node, T::Forall(*v, Box::new(t)))
            }
            C::TyApp(_, a) => {
                let (e, t) = parts.next().unwrap();
                let T::Forall(v, t) = t else {
                    panic!("checked Core instantiation")
                };
                (e.node, t.substitute(v, a))
            }
            C::Let(g, _) => {
                let bs = g
                    .binds
                    .iter()
                    .map(|b| (bind(&b.name), parts.next().unwrap().0))
                    .collect();
                let (b, t) = parts.next().unwrap();
                (Node::Let(g.recursive, bs, Box::new(b)), t)
            }
            C::Con(c, args, _) => {
                let d = &program.constructors[c.index as usize];
                let t = crate::elaborate::instantiate(
                    &d.result,
                    &d.parameters,
                    args,
                );
                (Node::Con(c.clone(), parts.map(|p| p.0).collect()), t)
            }
            C::Prim(op, ts, _, span) => {
                let (mut t, n) = crate::primitives::core_signature(
                    *op,
                    ts,
                    &program.data_types,
                )
                .expect("checked primitive");
                for _ in 0..n {
                    let T::Arrow(_, b) = t else { unreachable!() };
                    t = *b;
                }
                (Node::Prim(*op, parts.map(|p| p.0).collect(), *span), t)
            }
            C::Case(_, i, t, alts) => {
                let e = parts.next().unwrap().0;
                let alts = alts
                    .iter()
                    .map(|a| {
                        (
                            a.con.clone(),
                            a.binders.iter().map(&mut bind).collect(),
                            parts.next().unwrap().0,
                        )
                    })
                    .collect();
                (Node::Case(Box::new(e), bind(i), alts), t.clone())
            }
            C::Join(j, ps, _, _) => {
                let b = parts.next().unwrap().0;
                let (r, t) = parts.next().unwrap();
                (
                    Node::Join(
                        j.clone(),
                        ps.iter().map(&mut bind).collect(),
                        Box::new(b),
                        Box::new(r),
                    ),
                    t,
                )
            }
            C::Jump(j, _, t) => (
                Node::Jump(j.clone(), parts.map(|p| p.0).collect()),
                t.clone(),
            ),
            C::PatternFail(s, t) => (Node::Fail(*s), t.clone()),
            C::Match(_) => unreachable!(),
        };
        results.push((
            Expr {
                node,
                rep: representation(&ty),
            },
            ty,
        ));
    }
    results.pop().unwrap().0
}
/// Evidence is recorded while types exist. Later passes may only copy established facts.
pub fn erase(mut program: core::Program) -> Program {
    let mut reps = BTreeMap::new();
    let globals = program
        .groups
        .iter()
        .flat_map(|g| &g.binds)
        .map(|b| {
            reps.insert(b.name.name.clone(), representation(&b.name.ty));
            (b.name.name.clone(), expression(&b.rhs, &program, &mut reps))
        })
        .collect();
    program.global_types = program
        .groups
        .iter()
        .flat_map(|g| &g.binds)
        .map(|b| {
            (
                b.name.name.clone(),
                crate::core_type::fingerprint(&b.name.ty),
            )
        })
        .collect();
    // Consuming Core must not recursively destroy its nested expressions.
    let mut todo = program
        .groups
        .drain(..)
        .flat_map(|g| g.binds.into_iter().map(|b| *b.rhs))
        .collect::<Vec<_>>();
    while let Some(e) = todo.pop() {
        use core::Expr as C;
        match e {
            C::Lam(_, b) | C::TyLam(_, b) | C::TyApp(b, _) => todo.push(*b),
            C::App(a, b) | C::Join(_, _, a, b) => {
                todo.push(*a);
                todo.push(*b);
            }
            C::Let(g, b) => {
                todo.push(*b);
                todo.extend(g.binds.into_iter().map(|b| *b.rhs));
            }
            C::Con(_, _, es) | C::Prim(_, _, es, _) | C::Jump(_, es, _) => {
                todo.extend(es)
            }
            C::Case(e, _, _, alts) => {
                todo.push(*e);
                todo.extend(alts.into_iter().map(|a| a.rhs));
            }
            C::Match(_) => {
                unreachable!("match compilation precedes erasure")
            }
            _ => {}
        }
    }
    Program {
        globals,
        reps,
        metadata: program,
    }
}

pub(crate) fn literal_rep(l: &core::Lit) -> Rep {
    match l {
        core::Lit::Int(_) => Rep::Int,
        core::Lit::Double(_) => Rep::Double,
        _ => Rep::Word,
    }
}

/// Compact syntax uses an explicit print stack and bounded indentation. Debug's
/// indentation grows with depth and makes even a linear tree quadratic in bytes.
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        enum Work<'a> {
            Expr(&'a Expr),
            Text(String),
        }
        let mut todo = vec![Work::Expr(self)];
        while let Some(work) = todo.pop() {
            match work {
                Work::Text(s) => write!(f, "{s}")?,
                Work::Expr(e) => {
                    let mut parts = vec![];
                    match &e.node {
                        Node::Var(v) => write!(f, "{v}")?,
                        Node::Global(v) => write!(f, "@{v}")?,
                        Node::Lit(l) => write!(f, "{l}")?,
                        Node::Lam(v, b) => {
                            write!(f, "(lambda {v}. ")?;
                            parts.push(Work::Expr(b));
                            parts.push(Work::Text(")".into()));
                        }
                        Node::App(a, b) => {
                            write!(f, "(")?;
                            parts.extend([
                                Work::Expr(a),
                                Work::Text(" ".into()),
                                Work::Expr(b),
                                Work::Text(")".into()),
                            ]);
                        }
                        Node::Con(c, es) => {
                            write!(f, "({}", c.name)?;
                            for e in es {
                                parts.push(Work::Text(" ".into()));
                                parts.push(Work::Expr(e));
                            }
                            parts.push(Work::Text(")".into()));
                        }
                        Node::Prim(p, es, _) => {
                            write!(f, "({}", p.text())?;
                            for e in es {
                                parts.push(Work::Text(" ".into()));
                                parts.push(Work::Expr(e));
                            }
                            parts.push(Work::Text(")".into()));
                        }
                        Node::Jump(j, es) => {
                            write!(f, "(jump {j}")?;
                            for e in es {
                                parts.push(Work::Text(" ".into()));
                                parts.push(Work::Expr(e));
                            }
                            parts.push(Work::Text(")".into()));
                        }
                        Node::Let(rec, bs, b) => {
                            write!(
                                f,
                                "(let{} ",
                                if *rec { "rec" } else { "" }
                            )?;
                            for (v, e) in bs {
                                parts.push(Work::Text(format!("{v} = ")));
                                parts.push(Work::Expr(e));
                                parts.push(Work::Text(";\n".into()));
                            }
                            parts.extend([
                                Work::Text("in ".into()),
                                Work::Expr(b),
                                Work::Text(")".into()),
                            ]);
                        }
                        Node::Case(e, v, alts) => {
                            write!(f, "(case ")?;
                            parts.extend([
                                Work::Expr(e),
                                Work::Text(format!(" as {v} {{\n")),
                            ]);
                            for (c, ps, e) in alts {
                                let c = match c {
                                    core::AltCon::Con(c) => c.name.clone(),
                                    core::AltCon::Lit(l) => l.to_string(),
                                    core::AltCon::Default => "_".into(),
                                };
                                parts.push(Work::Text(c));
                                for p in ps {
                                    parts.push(Work::Text(format!(" {p}")));
                                }
                                parts.extend([
                                    Work::Text(" -> ".into()),
                                    Work::Expr(e),
                                    Work::Text(";\n".into()),
                                ]);
                            }
                            parts.push(Work::Text("})".into()));
                        }
                        Node::Join(j, ps, b, r) => {
                            write!(f, "(join {j}")?;
                            for p in ps {
                                write!(f, " {p}")?;
                            }
                            write!(f, " = ")?;
                            parts.extend([
                                Work::Expr(b),
                                Work::Text("\nin ".into()),
                                Work::Expr(r),
                                Work::Text(")".into()),
                            ]);
                        }
                        Node::Fail(s) => write!(f, "fail {s:?}")?,
                    }
                    todo.extend(parts.into_iter().rev());
                }
            }
        }
        Ok(())
    }
}
/// Destroy an owned erased tree without recursively following Box edges.
pub fn discard(root: Expr) {
    let mut todo = vec![root];
    while let Some(e) = todo.pop() {
        match e.node {
            Node::Lam(_, b) => todo.push(*b),
            Node::App(a, b) | Node::Join(_, _, a, b) => {
                todo.push(*a);
                todo.push(*b);
            }
            Node::Let(_, bs, b) => {
                todo.push(*b);
                todo.extend(bs.into_iter().map(|(_, e)| e));
            }
            Node::Con(_, es) | Node::Prim(_, es, _) | Node::Jump(_, es) => {
                todo.extend(es)
            }
            Node::Case(e, _, alts) => {
                todo.push(*e);
                todo.extend(alts.into_iter().map(|(_, _, e)| e));
            }
            _ => {}
        }
    }
}
