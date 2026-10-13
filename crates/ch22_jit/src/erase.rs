//! Remove System F abstractions and applications; dictionary values remain.
use crate::{ast::Op, core, data::ConId, names::Id, source::Span};

#[derive(Clone, Debug)]
/// Untyped Core retains strict evaluation order and lexical join points.
pub enum Expr {
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
/// Ordered globals and runtime constructor/location metadata.
pub struct Program {
    pub globals: Vec<(Id, Expr)>,
    pub metadata: core::Program,
}

/// Postorder erasure uses an explicit worklist, including long constructor spines.
fn expr(root: &core::Expr) -> Expr {
    use core::Expr as C;
    let mut todo = vec![(root, false)];
    let mut results: Vec<Expr> = vec![];
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

        let result = match e {
            C::Var(i) => Expr::Var(i.name.clone()),
            C::Global(i) => Expr::Global(i.name.clone()),
            C::Lit(l) => Expr::Lit(l.clone()),
            C::TyLam(..) | C::TyApp(..) => parts.next().unwrap(),
            C::Lam(i, _) => {
                Expr::Lam(i.name.clone(), Box::new(parts.next().unwrap()))
            }
            C::App(..) => Expr::App(
                Box::new(parts.next().unwrap()),
                Box::new(parts.next().unwrap()),
            ),
            C::Let(g, _) => {
                let bs = g
                    .binds
                    .iter()
                    .map(|b| (b.name.name.clone(), parts.next().unwrap()))
                    .collect();
                Expr::Let(g.recursive, bs, Box::new(parts.next().unwrap()))
            }
            C::Con(c, _, _) => Expr::Con(c.clone(), parts.collect()),
            C::Prim(p, _, _, s) => Expr::Prim(*p, parts.collect(), *s),
            C::Case(_, i, _, alts) => {
                let e = parts.next().unwrap();
                let alts = alts
                    .iter()
                    .map(|a| {
                        (
                            a.con.clone(),
                            a.binders
                                .iter()
                                .map(|i| i.name.clone())
                                .collect(),
                            parts.next().unwrap(),
                        )
                    })
                    .collect();
                Expr::Case(Box::new(e), i.name.clone(), alts)
            }
            C::Join(j, ps, _, _) => Expr::Join(
                j.clone(),
                ps.iter().map(|i| i.name.clone()).collect(),
                Box::new(parts.next().unwrap()),
                Box::new(parts.next().unwrap()),
            ),
            C::Jump(j, _, _) => Expr::Jump(j.clone(), parts.collect()),
            C::PatternFail(s, _) => Expr::Fail(*s),
            C::Match(_) => {
                unreachable!("match compilation precedes erasure")
            }
        };
        results.push(result);
    }
    results.pop().unwrap()
}

/// Erase executable types only; read-only metadata supports diagnostics and printers.
pub fn erase(mut program: core::Program) -> Program {
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
    let globals = program
        .groups
        .iter()
        .flat_map(|g| &g.binds)
        .map(|b| (b.name.name.clone(), expr(&b.rhs)))
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
        metadata: program,
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
                    match e {
                        Expr::Var(v) => write!(f, "{v}")?,
                        Expr::Global(v) => write!(f, "@{v}")?,
                        Expr::Lit(l) => write!(f, "{l}")?,
                        Expr::Lam(v, b) => {
                            write!(f, "(lambda {v}. ")?;
                            parts.push(Work::Expr(b));
                            parts.push(Work::Text(")".into()));
                        }
                        Expr::App(a, b) => {
                            write!(f, "(")?;
                            parts.extend([
                                Work::Expr(a),
                                Work::Text(" ".into()),
                                Work::Expr(b),
                                Work::Text(")".into()),
                            ]);
                        }
                        Expr::Con(c, es) => {
                            write!(f, "({}", c.name)?;
                            for e in es {
                                parts.push(Work::Text(" ".into()));
                                parts.push(Work::Expr(e));
                            }
                            parts.push(Work::Text(")".into()));
                        }
                        Expr::Prim(p, es, _) => {
                            write!(f, "({}", p.text())?;
                            for e in es {
                                parts.push(Work::Text(" ".into()));
                                parts.push(Work::Expr(e));
                            }
                            parts.push(Work::Text(")".into()));
                        }
                        Expr::Jump(j, es) => {
                            write!(f, "(jump {j}")?;
                            for e in es {
                                parts.push(Work::Text(" ".into()));
                                parts.push(Work::Expr(e));
                            }
                            parts.push(Work::Text(")".into()));
                        }
                        Expr::Let(rec, bs, b) => {
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
                        Expr::Case(e, v, alts) => {
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
                        Expr::Join(j, ps, b, r) => {
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
                        Expr::Fail(s) => write!(f, "fail {s:?}")?,
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
        match e {
            Expr::Lam(_, b) => todo.push(*b),
            Expr::App(a, b) | Expr::Join(_, _, a, b) => {
                todo.push(*a);
                todo.push(*b);
            }
            Expr::Let(_, bs, b) => {
                todo.push(*b);
                todo.extend(bs.into_iter().map(|(_, e)| e));
            }
            Expr::Con(_, es) | Expr::Prim(_, es, _) | Expr::Jump(_, es) => {
                todo.extend(es)
            }
            Expr::Case(e, _, alts) => {
                todo.push(*e);
                todo.extend(alts.into_iter().map(|(_, _, e)| e));
            }
            _ => {}
        }
    }
}
