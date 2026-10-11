//! Lift lambdas, classify call sites, and allocate recursive environments in two phases.
use crate::{
    anf::{self, Anf, Atom, Lambda, Var},
    anf_simplify::free_vars,
    core::Lit,
    flat::{self, Body, Flat, Func, FuncId, GlobalInit, Rhs},
    session::Session,
};
use std::{collections::BTreeMap, rc::Rc};
struct Info {
    id: FuncId,
    lambda: Lambda,
    captures: Vec<Var>,
    global: bool,
    escapes: bool,
}
struct Convert<'a> {
    sess: &'a mut Session,
    infos: BTreeMap<Var, Info>,
    order: Vec<Var>,
}
fn function(body: &anf::Body) -> Option<&Lambda> {
    if let Anf::Let(v, anf::Rhs::Lambda(l), b) = body.as_ref()
        && matches!(b.as_ref(),Anf::Ret(Atom::Var(w)) if v==w)
    {
        Some(l)
    } else {
        None
    }
}
impl Convert<'_> {
    fn register(&mut self, v: &Var, l: &Lambda, global: bool) {
        let mut captures = free_vars(&l.body);
        for p in &l.params {
            captures.remove(p);
        }
        assert!(!global || captures.is_empty(), "closed global lambda");
        let id = FuncId(self.order.len() as u32);
        self.order.push(v.clone());
        self.infos.insert(
            v.clone(),
            Info {
                id,
                lambda: l.clone(),
                captures: captures.into_iter().collect(),
                global,
                escapes: false,
            },
        );
    }
    fn collect(&mut self, b: &anf::Body) {
        let mut todo = vec![b];
        while let Some(b) = todo.pop() {
            match b.as_ref() {
                Anf::Let(v, anf::Rhs::Lambda(l), b) => {
                    self.register(v, l, false);
                    todo.push(b);
                    todo.push(&l.body);
                }
                Anf::Let(_, _, b) => todo.push(b),
                Anf::LetRec(bs, b) => {
                    todo.push(b);
                    for (v, l) in bs {
                        self.register(v, l, false);
                        todo.push(&l.body);
                    }
                }
                Anf::Join(_, _, b, r) => {
                    todo.push(r);
                    todo.push(b);
                }
                Anf::Case(_, alts) => {
                    todo.extend(alts.iter().rev().map(|(_, _, b)| b))
                }
                _ => {}
            }
        }
    }
    fn use_atom(&mut self, a: &Atom, arity: Option<usize>) {
        if let Atom::Var(v) | Atom::Global(v) = a
            && let Some(info) = self.infos.get_mut(v)
            && arity != Some(info.lambda.params.len())
        {
            info.escapes = true;
        }
    }
    fn args(&mut self, args: &[Atom]) {
        for a in args {
            self.use_atom(a, None);
        }
    }
    fn escapes(&mut self, b: &anf::Body) {
        let mut todo = vec![b];
        while let Some(b) = todo.pop() {
            match b.as_ref() {
                Anf::Ret(a) => self.use_atom(a, None),
                Anf::Jump(_, args) => self.args(args),
                Anf::TailCall(a, args) => {
                    self.use_atom(a, Some(args.len()));
                    self.args(args);
                }
                Anf::Let(_, r, b) => {
                    match r {
                        anf::Rhs::Atom(a) | anf::Rhs::Field(a, _) => {
                            self.use_atom(a, None)
                        }
                        anf::Rhs::Call(a, args) => {
                            self.use_atom(a, Some(args.len()));
                            self.args(args);
                        }
                        anf::Rhs::Con(_, args)
                        | anf::Rhs::Prim(_, args, _) => self.args(args),
                        anf::Rhs::Lambda(l) => todo.push(&l.body),
                    }
                    todo.push(b);
                }
                Anf::LetRec(bs, b) => {
                    for (_, l) in bs {
                        todo.push(&l.body);
                    }
                    todo.push(b);
                }
                Anf::Join(_, _, b, r) => {
                    todo.push(b);
                    todo.push(r);
                }
                Anf::Case(a, alts) => {
                    self.use_atom(a, None);
                    for (_, _, b) in alts {
                        todo.push(b);
                    }
                }
                Anf::Fail(_) => {}
            }
        }
    }
    fn known(&self, a: &Atom, n: usize) -> Option<(FuncId, Option<Atom>)> {
        let (Atom::Var(v) | Atom::Global(v)) = a else {
            return None;
        };
        let info = self.infos.get(v)?;
        (n == info.lambda.params.len() && (info.global || !info.escapes))
            .then(|| (info.id, (!info.global).then(|| a.clone())))
    }
    fn rhs(&self, v: &Var, r: &anf::Rhs) -> Rhs {
        match r {
            anf::Rhs::Atom(a) => Rhs::Atom(a.clone()),
            anf::Rhs::Field(a, i) => Rhs::Field(a.clone(), *i),
            anf::Rhs::Con(c, args) => {
                Rhs::AllocCon(c.clone(), args.clone())
            }
            anf::Rhs::Prim(p, args, s) => Rhs::Prim(*p, args.clone(), *s),
            anf::Rhs::Call(a, args) => match self.known(a, args.len()) {
                Some((f, env)) => Rhs::CallKnown(f, env, args.clone()),
                None => Rhs::CallUnknown(a.clone(), args.clone()),
            },
            anf::Rhs::Lambda(_) => {
                let info = &self.infos[v];
                Rhs::AllocClosure(
                    info.id,
                    info.captures.iter().cloned().map(Atom::Var).collect(),
                )
            }
        }
    }
    fn body(&mut self, b: &anf::Body) -> Body {
        let mut todo = vec![(b, false)];
        let mut out: Vec<Body> = vec![];
        while let Some((b, done)) = todo.pop() {
            if !done {
                todo.push((b, true));
                match b.as_ref() {
                    Anf::Let(_, _, b) | Anf::LetRec(_, b) => {
                        todo.push((b, false))
                    }
                    Anf::Join(_, _, b, r) => {
                        todo.push((r, false));
                        todo.push((b, false));
                    }
                    Anf::Case(_, alts) => todo.extend(
                        alts.iter().rev().map(|(_, _, b)| (b, false)),
                    ),
                    _ => {}
                }
                continue;
            }
            let node = match b.as_ref() {
                Anf::Ret(a) => Flat::Ret(a.clone()),
                Anf::Fail(s) => Flat::Fail(*s),
                Anf::Jump(j, args) => Flat::Jump(j.clone(), args.clone()),
                Anf::TailCall(a, args) => match self.known(a, args.len()) {
                    Some((f, env)) => {
                        Flat::TailCallKnown(f, env, args.clone())
                    }
                    None => Flat::TailCallUnknown(a.clone(), args.clone()),
                },
                Anf::Let(v, r, _) => {
                    Flat::Let(v.clone(), self.rhs(v, r), out.pop().unwrap())
                }
                Anf::Case(a, alts) => {
                    let bodies = out.split_off(out.len() - alts.len());
                    Flat::Case(
                        a.clone(),
                        alts.iter()
                            .zip(bodies)
                            .map(|((c, ps, _), b)| {
                                (c.clone(), ps.clone(), b)
                            })
                            .collect(),
                    )
                }
                Anf::Join(j, ps, _, _) => {
                    let r = out.pop().unwrap();
                    let b = out.pop().unwrap();
                    Flat::Join(j.clone(), ps.clone(), b, r)
                }
                Anf::LetRec(bs, _) => {
                    let mut body = out.pop().unwrap();
                    for (v, _) in bs.iter().rev() {
                        for (index, capture) in
                            self.infos[v].captures.iter().enumerate().rev()
                        {
                            let temp = self.sess.fresh_id("$patch");
                            body = Rc::new(Flat::Let(
                                temp,
                                Rhs::PatchClosure(
                                    Atom::Var(v.clone()),
                                    index as u32,
                                    Atom::Var(capture.clone()),
                                ),
                                body,
                            ));
                        }
                    }
                    for (v, _) in bs.iter().rev() {
                        let info = &self.infos[v];
                        body = Rc::new(Flat::Let(
                            v.clone(),
                            Rhs::AllocClosure(
                                info.id,
                                vec![
                                    Atom::Lit(Lit::Int(0));
                                    info.captures.len()
                                ],
                            ),
                            body,
                        ));
                    }
                    out.push(body);
                    continue;
                }
            };
            out.push(Rc::new(node));
        }
        out.pop().unwrap()
    }
}
/// Every function is closed over explicit parameters, globals, and environment loads.
pub fn convert(sess: &mut Session, program: anf::Program) -> flat::Program {
    let mut c = Convert {
        sess,
        infos: BTreeMap::new(),
        order: vec![],
    };
    for (v, b) in &program.globals {
        if let Some(l) = function(b) {
            c.register(v, l, true);
            c.collect(&l.body);
        } else {
            c.collect(b);
        }
    }
    for (_, b) in &program.globals {
        c.escapes(b);
    }
    let mut funcs = vec![];
    for v in c.order.clone() {
        let info = &c.infos[&v];
        let id = info.id;
        let params = info.lambda.params.clone();
        let captures = info.captures.clone();
        let global = info.global;
        let source = info.lambda.body.clone();
        let env = (!global).then(|| c.sess.fresh_id("$env"));
        let mut body = c.body(&source);
        if let Some(env) = &env {
            for (i, capture) in captures.iter().enumerate().rev() {
                body = Rc::new(Flat::Let(
                    capture.clone(),
                    Rhs::EnvField(env.clone(), i as u32),
                    body,
                ));
            }
        }
        funcs.push(Func {
            name: id,
            source: v,
            env,
            params,
            body,
            captures,
        });
    }
    let globals = program
        .globals
        .iter()
        .map(|(v, b)| {
            if let Some(info) = c.infos.get(v).filter(|i| i.global) {
                GlobalInit {
                    name: v.clone(),
                    function: Some(info.id),
                    body: Rc::new(Flat::Ret(Atom::Global(v.clone()))),
                }
            } else {
                GlobalInit {
                    name: v.clone(),
                    function: None,
                    body: c.body(b),
                }
            }
        })
        .collect();
    flat::Program {
        funcs,
        globals,
        metadata: program.metadata,
    }
}
