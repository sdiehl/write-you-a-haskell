//! A-normal form: each operation consumes atoms; non-tail cases share a join.
use crate::{
    ast::Op,
    core::{self, AltCon, Lit},
    data::ConId,
    erase,
    names::Id,
    session::Session,
    source::Span,
};
use std::{collections::BTreeMap, rc::Rc};
/// An erased binder keeps its unique identity and diagnostic spelling.
pub type Var = Id;
/// Syntax is immutable and shared between closures and machine states.
pub type Body = Rc<Anf>;
#[derive(Clone, Debug, PartialEq, Eq)]
/// Atoms cannot perform work or fail when read from a well-scoped environment.
pub enum Atom {
    Var(Var),
    Global(Id),
    Lit(Lit),
}
#[derive(Clone, Debug)]
/// Lambda parameters are merged only across syntactically adjacent lambdas.
pub struct Lambda {
    pub params: Vec<Var>,
    pub body: Body,
}
#[derive(Clone, Debug)]
/// One strict computation whose operands have already been evaluated.
pub enum Rhs {
    Atom(Atom),
    Call(Atom, Vec<Atom>),
    Con(ConId, Vec<Atom>),
    Prim(Op, Vec<Atom>, Span),
    Lambda(Lambda),
    Field(Atom, u32),
}
#[derive(Clone, Debug)]
/// Jumps transfer within a function; tail calls replace its activation.
pub enum Anf {
    Let(Var, Rhs, Body),
    LetRec(Vec<(Var, Lambda)>, Body),
    Join(Id, Vec<Var>, Body, Body),
    Jump(Id, Vec<Atom>),
    Case(Atom, Vec<(AltCon, Vec<Var>, Body)>),
    Ret(Atom),
    TailCall(Atom, Vec<Atom>),
    Fail(Span),
}
#[derive(Clone, Debug)]
/// Global initialization order is preserved from typed Core.
pub struct Program {
    pub globals: Vec<(Id, Body)>,
    pub metadata: core::Program,
    pub reps: BTreeMap<Var, crate::rep::Rep>,
}
#[derive(Clone)]
enum Context {
    Return,
    Jump(Id),
    Bind(Var, Body),
}
struct Convert<'a> {
    sess: &'a mut Session,
    arities: BTreeMap<Id, usize>,
    reps: BTreeMap<Var, crate::rep::Rep>,
}
fn arity(mut e: &erase::Expr) -> usize {
    let mut n = 0;
    while let erase::Node::Lam(_, b) = &e.node {
        n += 1;
        e = b;
    }
    n
}
fn collect(e: &erase::Expr, out: &mut BTreeMap<Id, usize>) {
    use erase::Node as E;
    let mut todo = vec![e];
    while let Some(e) = todo.pop() {
        match &e.node {
            E::Let(_, bs, b) => {
                for (v, e) in bs {
                    let n = arity(e);
                    if n > 0 {
                        out.insert(v.clone(), n);
                    }
                    todo.push(e);
                }
                todo.push(b);
            }
            E::Lam(_, b) => todo.push(b),
            E::App(a, b) | E::Join(_, _, a, b) => {
                todo.push(a);
                todo.push(b);
            }
            E::Con(_, es) | E::Prim(_, es, _) | E::Jump(_, es) => {
                todo.extend(es)
            }
            E::Case(e, _, alts) => {
                todo.push(e);
                todo.extend(alts.iter().map(|(_, _, e)| e));
            }
            _ => {}
        }
    }
}
impl Convert<'_> {
    fn fresh(&mut self, rep: crate::rep::Rep) -> Var {
        let id = self.sess.fresh_id("$a");
        self.reps.insert(id.clone(), rep);
        id
    }
    fn finish(&mut self, a: Atom, k: Context) -> Body {
        Rc::new(match k {
            Context::Return => Anf::Ret(a),
            Context::Jump(j) => Anf::Jump(j, vec![a]),
            Context::Bind(v, b) => Anf::Let(v, Rhs::Atom(a), b),
        })
    }
    fn rhs(&mut self, r: Rhs, k: Context, rep: crate::rep::Rep) -> Body {
        if let Context::Bind(v, b) = k {
            return Rc::new(Anf::Let(v, r, b));
        }
        if let (Rhs::Call(f, args), Context::Return) = (&r, &k) {
            return Rc::new(Anf::TailCall(f.clone(), args.clone()));
        }
        let v = self.fresh(rep);
        let rest = self.finish(Atom::Var(v.clone()), k);
        Rc::new(Anf::Let(v, r, rest))
    }
    /// Tasks hold pending compiler work. Neither expression depth nor the
    /// number of operands adds a Rust call frame.
    fn expr(&mut self, e: erase::Expr, k: Context) -> Body {
        use crate::rep::Rep;
        use erase::Node as E;
        enum Work {
            Expr(erase::Expr, Context),
            Bindings(Vec<(Var, erase::Expr)>),
            Lambda(Vec<Var>, Context, Rep),
            Rec(Vec<(Var, Vec<Var>)>),
            Case(
                erase::Expr,
                Var,
                Vec<(AltCon, Vec<Var>)>,
                Option<(Id, Var, Body)>,
            ),
            Join(Id, Vec<Var>, Option<(Id, Var, Body)>),
            Call(erase::Expr, Var, Vec<erase::Expr>, Var, Rep),
        }
        fn peel(mut e: erase::Expr) -> (Vec<Var>, erase::Expr) {
            let mut ps = vec![];
            while let E::Lam(v, b) = e.node {
                ps.push(v);
                e = *b;
            }
            (ps, e)
        }
        fn branch(
            k: Context,
            sess: &mut Session,
        ) -> (Context, Option<(Id, Var, Body)>) {
            match k {
                Context::Bind(v, b) => {
                    let j = sess.fresh_id("$cont");
                    (Context::Jump(j.clone()), Some((j, v, b)))
                }
                k => (k, None),
            }
        }
        fn wrap(body: Body, cont: Option<(Id, Var, Body)>) -> Body {
            match cont {
                Some((j, v, b)) => Rc::new(Anf::Join(j, vec![v], b, body)),
                None => body,
            }
        }
        let mut todo = vec![Work::Expr(e, k)];
        let mut results: Vec<Body> = vec![];
        while let Some(work) = todo.pop() {
            match work {
                Work::Bindings(mut bs) => {
                    if let Some((v, e)) = bs.pop() {
                        let b = results.pop().unwrap();
                        todo.push(Work::Bindings(bs));
                        todo.push(Work::Expr(e, Context::Bind(v, b)));
                    }
                }
                Work::Lambda(params, k, rep) => {
                    let body = results.pop().unwrap();
                    results.push(self.rhs(
                        Rhs::Lambda(Lambda { params, body }),
                        k,
                        rep,
                    ));
                }
                Work::Rec(bs) => {
                    let bodies =
                        results.split_off(results.len() - bs.len());
                    let rest = results.pop().unwrap();
                    let bs = bs
                        .into_iter()
                        .zip(bodies)
                        .map(|((v, params), body)| {
                            (v, Lambda { params, body })
                        })
                        .collect();
                    results.push(Rc::new(Anf::LetRec(bs, rest)));
                }
                Work::Case(e, v, heads, cont) => {
                    let bodies =
                        results.split_off(results.len() - heads.len());
                    let alts = heads
                        .into_iter()
                        .zip(bodies)
                        .map(|((c, ps), b)| (c, ps, b))
                        .collect();
                    let b = wrap(
                        Rc::new(Anf::Case(Atom::Var(v.clone()), alts)),
                        cont,
                    );
                    todo.push(Work::Expr(e, Context::Bind(v, b)));
                }
                Work::Join(j, ps, cont) => {
                    let rest = results.pop().unwrap();
                    let body = results.pop().unwrap();
                    results.push(wrap(
                        Rc::new(Anf::Join(j, ps, body, rest)),
                        cont,
                    ));
                }
                Work::Call(head, f, args, result, rep) => {
                    let rest = results.pop().unwrap();
                    let vars: Vec<_> =
                        args.iter().map(|e| self.fresh(e.rep)).collect();
                    let atoms =
                        vars.iter().cloned().map(Atom::Var).collect();
                    let b = self.rhs(
                        Rhs::Call(Atom::Var(f.clone()), atoms),
                        Context::Bind(result, rest),
                        rep,
                    );
                    let bs = std::iter::once((f, head))
                        .chain(vars.into_iter().zip(args))
                        .collect();
                    results.push(b);
                    todo.push(Work::Bindings(bs));
                }
                Work::Expr(e, k) => {
                    let rep = e.rep;
                    match e.node {
                        E::Var(v) => {
                            results.push(self.finish(Atom::Var(v), k))
                        }
                        E::Global(v) => {
                            results.push(self.finish(Atom::Global(v), k))
                        }
                        E::Lit(l) => {
                            results.push(self.finish(Atom::Lit(l), k))
                        }
                        node @ E::Lam(..) => {
                            let (ps, body) =
                                peel(erase::Expr { node, rep });
                            todo.push(Work::Lambda(ps, k, rep));
                            todo.push(Work::Expr(body, Context::Return));
                        }
                        node @ E::App(..) => {
                            let mut head = erase::Expr { node, rep };
                            let mut args = vec![];
                            let mut reps = vec![];
                            while let E::App(f, a) = head.node {
                                reps.push(head.rep);
                                args.push(*a);
                                head = *f;
                            }
                            args.reverse();
                            reps.reverse();
                            let known = match &head.node {
                                E::Var(v) | E::Global(v) => {
                                    self.arities.get(v).copied()
                                }
                                E::Lam(..) => Some(arity(&head)),
                                _ => None,
                            };
                            // Unknown calls execute before evaluating their next argument.
                            let n = known.unwrap_or(1).min(args.len());
                            let extra = args.split_off(n);
                            let extra_reps = reps.split_off(n);
                            let f = self.fresh(head.rep);
                            if extra.is_empty() {
                                let vars: Vec<_> = args
                                    .iter()
                                    .map(|e| self.fresh(e.rep))
                                    .collect();
                                let atoms = vars
                                    .iter()
                                    .cloned()
                                    .map(Atom::Var)
                                    .collect();
                                results.push(self.rhs(
                                    Rhs::Call(Atom::Var(f.clone()), atoms),
                                    k,
                                    rep,
                                ));
                                todo.push(Work::Bindings(
                                    std::iter::once((f, head))
                                        .chain(vars.into_iter().zip(args))
                                        .collect(),
                                ));
                            } else {
                                let result = self.fresh(Rep::Word);
                                let application =
                                    extra.into_iter().zip(extra_reps).fold(
                                        erase::Expr {
                                            node: E::Var(result.clone()),
                                            rep: Rep::Word,
                                        },
                                        |f, (a, rep)| erase::Expr {
                                            node: E::App(
                                                Box::new(f),
                                                Box::new(a),
                                            ),
                                            rep,
                                        },
                                    );
                                todo.push(Work::Call(
                                    head,
                                    f,
                                    args,
                                    result,
                                    Rep::Word,
                                ));
                                todo.push(Work::Expr(application, k));
                            }
                        }
                        E::Let(rec, bs, b) => {
                            if rec {
                                let (heads, bodies): (Vec<_>, Vec<_>) = bs
                                    .into_iter()
                                    .map(|(v, e)| {
                                        let (ps, e) = peel(e);
                                        ((v, ps), e)
                                    })
                                    .unzip();
                                todo.push(Work::Rec(heads));
                                todo.extend(bodies.into_iter().rev().map(
                                    |e| Work::Expr(e, Context::Return),
                                ));
                            } else {
                                todo.push(Work::Bindings(bs));
                            }
                            todo.push(Work::Expr(*b, k));
                        }
                        node @ (E::Con(..) | E::Prim(..) | E::Jump(..)) => {
                            let args = match &node {
                                E::Con(_, es)
                                | E::Prim(_, es, _)
                                | E::Jump(_, es) => es,
                                _ => unreachable!(),
                            };
                            let vars: Vec<_> = args
                                .iter()
                                .map(|e| self.fresh(e.rep))
                                .collect();
                            let atoms = vars
                                .iter()
                                .cloned()
                                .map(Atom::Var)
                                .collect();
                            let (args, body) = match node {
                                E::Con(c, es) => (
                                    es,
                                    self.rhs(Rhs::Con(c, atoms), k, rep),
                                ),
                                E::Prim(p, es, s) => (
                                    es,
                                    self.rhs(
                                        Rhs::Prim(p, atoms, s),
                                        k,
                                        rep,
                                    ),
                                ),
                                E::Jump(j, es) => {
                                    (es, Rc::new(Anf::Jump(j, atoms)))
                                }
                                _ => unreachable!(),
                            };
                            results.push(body);
                            todo.push(Work::Bindings(
                                vars.into_iter().zip(args).collect(),
                            ));
                        }
                        E::Case(e, v, alts) => {
                            let (k, cont) = branch(k, self.sess);
                            let (heads, bodies): (Vec<_>, Vec<_>) = alts
                                .into_iter()
                                .map(|(c, ps, b)| ((c, ps), b))
                                .unzip();
                            todo.push(Work::Case(*e, v, heads, cont));
                            todo.extend(
                                bodies
                                    .into_iter()
                                    .rev()
                                    .map(|e| Work::Expr(e, k.clone())),
                            );
                        }
                        E::Join(j, ps, b, r) => {
                            let (k, cont) = branch(k, self.sess);
                            todo.push(Work::Join(j, ps, cont));
                            todo.push(Work::Expr(*r, k.clone()));
                            todo.push(Work::Expr(*b, k));
                        }
                        E::Fail(s) => results.push(Rc::new(Anf::Fail(s))),
                    }
                }
            }
        }
        assert_eq!(results.len(), 1);
        results.pop().unwrap()
    }
}
/// Normalize left-to-right, sharing every non-tail continuation exactly once.
pub fn convert(sess: &mut Session, program: erase::Program) -> Program {
    let mut arities = BTreeMap::new();
    for (v, e) in &program.globals {
        if arity(e) > 0 {
            arities.insert(v.clone(), arity(e));
        }
        collect(e, &mut arities);
    }
    let mut c = Convert {
        sess,
        arities,
        reps: program.reps,
    };
    let globals = program
        .globals
        .into_iter()
        .map(|(v, e)| (v, c.expr(e, Context::Return)))
        .collect();
    Program {
        reps: c.reps,
        globals,
        metadata: program.metadata,
    }
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(v) => write!(f, "{v}"),
            Self::Global(v) => write!(f, "@{v}"),
            Self::Lit(l) => write!(f, "{l}"),
        }
    }
}
fn list<T: std::fmt::Display>(items: &[T]) -> String {
    items
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}
impl std::fmt::Display for Rhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(a) => write!(f, "{a}"),
            Self::Call(a, args) => write!(f, "call {a}({})", list(args)),
            Self::Con(c, args) => write!(f, "{}({})", c.name, list(args)),
            Self::Prim(p, args, _) => {
                write!(f, "{}({})", p.text(), list(args))
            }
            Self::Lambda(l) => {
                write!(f, "lambda ({}) {{ {} }}", list(&l.params), l.body)
            }
            Self::Field(a, i) => write!(f, "field {a}[{i}]"),
        }
    }
}
impl std::fmt::Display for Anf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        enum Work<'a> {
            Body(&'a Anf),
            Rhs(&'a Rhs),
            Text(String),
        }
        let mut todo = vec![Work::Body(self)];
        while let Some(work) = todo.pop() {
            let mut parts = vec![];
            match work {
                Work::Text(s) => write!(f, "{s}")?,
                Work::Rhs(Rhs::Lambda(l)) => {
                    write!(f, "lambda ({}) {{ ", list(&l.params))?;
                    parts.extend([
                        Work::Body(&l.body),
                        Work::Text(" }".into()),
                    ]);
                }
                Work::Rhs(r) => write!(f, "{r}")?,
                Work::Body(b) => match b {
                    Self::Let(v, r, b) => {
                        write!(f, "let {v} = ")?;
                        parts.extend([
                            Work::Rhs(r),
                            Work::Text(";\n".into()),
                            Work::Body(b),
                        ]);
                    }
                    Self::LetRec(bs, b) => {
                        for (v, l) in bs {
                            parts.extend([
                                Work::Text(format!(
                                    "rec {v}({}) = {{ ",
                                    list(&l.params)
                                )),
                                Work::Body(&l.body),
                                Work::Text(" }\n".into()),
                            ]);
                        }
                        parts.push(Work::Body(b));
                    }
                    Self::Join(j, ps, b, r) => {
                        write!(f, "join {j}({}) = {{ ", list(ps))?;
                        parts.extend([
                            Work::Body(b),
                            Work::Text(" };\n".into()),
                            Work::Body(r),
                        ]);
                    }
                    Self::Jump(j, args) => {
                        write!(f, "jump {j}({})", list(args))?
                    }
                    Self::Case(a, alts) => {
                        writeln!(f, "case {a} {{")?;
                        for (c, ps, b) in alts {
                            let head = match c {
                                AltCon::Con(c) => c.name.clone(),
                                AltCon::Lit(l) => l.to_string(),
                                AltCon::Default => "_".into(),
                            };
                            parts.extend([
                                Work::Text(format!(
                                    "{head}({}) -> {{ ",
                                    list(ps)
                                )),
                                Work::Body(b),
                                Work::Text(" }\n".into()),
                            ]);
                        }
                        parts.push(Work::Text("}".into()));
                    }
                    Self::Ret(a) => write!(f, "return {a}")?,
                    Self::TailCall(fun, args) => {
                        write!(f, "tail {fun}({})", list(args))?
                    }
                    Self::Fail(s) => write!(f, "fail {s:?}")?,
                },
            }
            todo.extend(parts.into_iter().rev());
        }
        Ok(())
    }
}

/// Drain child edges explicitly when the last owner releases a deep ANF tree.
impl Drop for Anf {
    fn drop(&mut self) {
        fn edges(node: &mut Anf, todo: &mut Vec<Body>) {
            fn take(b: &mut Body) -> Body {
                std::mem::replace(
                    b,
                    Rc::new(Anf::Ret(Atom::Lit(Lit::Int(0)))),
                )
            }
            match node {
                Anf::Let(_, r, b) => {
                    todo.push(take(b));
                    if let Rhs::Lambda(l) = r {
                        todo.push(take(&mut l.body));
                    }
                }
                Anf::LetRec(bs, b) => {
                    todo.push(take(b));
                    todo.extend(bs.drain(..).map(|(_, l)| l.body));
                }
                Anf::Join(_, _, b, r) => {
                    todo.push(take(b));
                    todo.push(take(r));
                }
                Anf::Case(_, alts) => {
                    todo.extend(alts.drain(..).map(|(_, _, b)| b))
                }
                _ => {}
            }
        }
        let mut todo = vec![];
        edges(self, &mut todo);
        while let Some(b) = todo.pop() {
            if let Ok(mut node) = Rc::try_unwrap(b) {
                edges(&mut node, &mut todo);
                // Empty this node before Drop runs again on the local variable.
                // Its remaining children are only leaf placeholders.
            }
        }
    }
}
