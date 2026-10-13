//! Structural checks after types have been erased.
use crate::anf::{Anf, Atom, Body, Program, Rhs, Var};
use std::collections::{BTreeMap, BTreeSet};

struct Check {
    globals: BTreeSet<Var>,
    seen: BTreeSet<Var>,
}

impl Check {
    fn bind(&mut self, v: &Var) -> Result<(), String> {
        self.unique(v)
    }
    fn unique(&mut self, v: &Var) -> Result<(), String> {
        if self.seen.insert(v.clone()) {
            Ok(())
        } else {
            Err(format!("duplicate ANF binder {v}"))
        }
    }
    fn atom(&self, a: &Atom, scope: &BTreeSet<Var>) -> Result<(), String> {
        match a {
            Atom::Var(v) if !scope.contains(v) => {
                Err(format!("unbound ANF variable {v}"))
            }
            Atom::Global(v) if !self.globals.contains(v) => {
                Err(format!("unbound ANF global {v}"))
            }
            _ => Ok(()),
        }
    }
    fn atoms(
        &self,
        args: &[Atom],
        scope: &BTreeSet<Var>,
    ) -> Result<(), String> {
        for a in args {
            self.atom(a, scope)?;
        }
        Ok(())
    }
    fn body(
        &mut self,
        b: &Body,
        scope: BTreeSet<Var>,
        joins: BTreeMap<Var, usize>,
    ) -> Result<(), String> {
        let mut todo = vec![(b, scope, joins)];
        while let Some((b, mut scope, mut joins)) = todo.pop() {
            match b.as_ref() {
                Anf::Ret(a) => self.atom(a, &scope)?,
                Anf::TailCall(f, args) => {
                    self.atom(f, &scope)?;
                    self.atoms(args, &scope)?;
                }
                Anf::Fail(_) => {}
                Anf::Jump(j, args) => {
                    if joins.get(j) != Some(&args.len()) {
                        return Err(
                            "unbound or unsaturated ANF join".into()
                        );
                    }
                    self.atoms(args, &scope)?;
                }
                Anf::Let(v, r, rest) => {
                    let lambda = match r {
                        Rhs::Atom(a) | Rhs::Field(a, _) => {
                            self.atom(a, &scope)?;
                            None
                        }
                        Rhs::Con(_, args) | Rhs::Prim(_, args, _) => {
                            self.atoms(args, &scope)?;
                            None
                        }
                        Rhs::Call(f, args) => {
                            self.atom(f, &scope)?;
                            self.atoms(args, &scope)?;
                            None
                        }
                        Rhs::Lambda(l) => Some((l, scope.clone())),
                    };
                    self.bind(v)?;
                    scope.insert(v.clone());
                    todo.push((rest, scope, joins));
                    if let Some((l, mut inner)) = lambda {
                        if l.params.is_empty() {
                            return Err("zero-arity ANF lambda".into());
                        }
                        for p in &l.params {
                            self.bind(p)?;
                            inner.insert(p.clone());
                        }
                        todo.push((&l.body, inner, BTreeMap::new()));
                    }
                }
                Anf::LetRec(bs, rest) => {
                    for (v, _) in bs {
                        self.bind(v)?;
                        scope.insert(v.clone());
                    }
                    for (_, l) in bs {
                        if l.params.is_empty() {
                            return Err("zero-arity ANF lambda".into());
                        }
                        let mut inner = scope.clone();
                        for p in &l.params {
                            self.bind(p)?;
                            inner.insert(p.clone());
                        }
                        todo.push((&l.body, inner, BTreeMap::new()));
                    }
                    todo.push((rest, scope, joins));
                }
                Anf::Join(j, params, body, rest) => {
                    self.unique(j)?;
                    let mut inner = scope.clone();
                    for p in params {
                        self.bind(p)?;
                        inner.insert(p.clone());
                    }
                    todo.push((body, inner, joins.clone()));
                    joins.insert(j.clone(), params.len());
                    todo.push((rest, scope, joins));
                }
                Anf::Case(a, alts) => {
                    self.atom(a, &scope)?;
                    for (_, ps, b) in alts {
                        let mut inner = scope.clone();
                        for p in ps {
                            self.bind(p)?;
                            inner.insert(p.clone());
                        }
                        todo.push((b, inner, joins.clone()));
                    }
                }
            }
        }
        Ok(())
    }
}

/// Check lexical scope, unique binders and fully applied local transfers.
pub fn lint(program: &Program) -> Result<(), String> {
    let globals = program
        .globals
        .iter()
        .map(|(v, _)| v.clone())
        .collect::<BTreeSet<_>>();
    if globals.len() != program.globals.len() {
        return Err("duplicate ANF global".into());
    }
    let mut c = Check {
        seen: globals.clone(),
        globals,
    };
    for (_, b) in &program.globals {
        c.body(b, BTreeSet::new(), BTreeMap::new())?;
    }
    Ok(())
}
