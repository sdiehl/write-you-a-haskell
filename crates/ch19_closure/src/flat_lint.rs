//! Structural checks after types have been erased.
use crate::flat::{Atom, Body, Flat, FuncId, Program, Rhs, Var};
use std::collections::{BTreeMap, BTreeSet};
struct Check<'a> {
    program: &'a Program,
    environment: Option<(Var, usize)>,
    globals: BTreeSet<Var>,
    seen: BTreeSet<Var>,
}
impl Check<'_> {
    fn bind(&mut self, v: &Var) -> Result<(), String> {
        if self.seen.insert(v.clone()) {
            Ok(())
        } else {
            Err(format!("duplicate Flat binder {v}"))
        }
    }
    fn atom(&self, a: &Atom, scope: &BTreeSet<Var>) -> Result<(), String> {
        match a {
            Atom::Var(v) if !scope.contains(v) => {
                Err(format!("unbound Flat variable {v}"))
            }
            Atom::Global(v) if !self.globals.contains(v) => {
                Err(format!("unbound Flat global {v}"))
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
    fn function(&self, id: FuncId) -> Result<&crate::flat::Func, String> {
        self.program
            .funcs
            .get(id.0 as usize)
            .filter(|f| f.name == id)
            .ok_or_else(|| format!("unknown Flat function {id}"))
    }
    fn known(
        &self,
        id: FuncId,
        env: &Option<Atom>,
        args: &[Atom],
        scope: &BTreeSet<Var>,
    ) -> Result<(), String> {
        let f = self.function(id)?;
        if f.params.len() != args.len() || f.env.is_some() != env.is_some()
        {
            return Err(
                "unsaturated Flat known call or wrong environment".into()
            );
        }
        if let Some(a) = env {
            self.atom(a, scope)?;
        }
        self.atoms(args, scope)
    }
    fn body(
        &mut self,
        b: &Body,
        mut scope: BTreeSet<Var>,
        mut joins: BTreeMap<Var, usize>,
    ) -> Result<(), String> {
        match b.as_ref() {
            Flat::Ret(a) => self.atom(a, &scope),
            Flat::TailCallUnknown(f, args) => {
                self.atom(f, &scope)?;
                self.atoms(args, &scope)
            }
            Flat::TailCallKnown(f, env, args) => {
                self.known(*f, env, args, &scope)
            }
            Flat::Fail(_) => Ok(()),
            Flat::Jump(j, args) => {
                if joins.get(j) != Some(&args.len()) {
                    return Err("unbound or unsaturated Flat join".into());
                }
                self.atoms(args, &scope)
            }
            Flat::Let(v, r, b) => {
                match r {
                    Rhs::Atom(a) | Rhs::Field(a, _) => {
                        self.atom(a, &scope)?
                    }
                    Rhs::AllocCon(_, args) | Rhs::Prim(_, args, _) => {
                        self.atoms(args, &scope)?
                    }
                    Rhs::CallUnknown(f, args) => {
                        self.atom(f, &scope)?;
                        self.atoms(args, &scope)?;
                    }
                    Rhs::CallKnown(f, env, args) => {
                        self.known(*f, env, args, &scope)?
                    }
                    Rhs::AllocClosure(f, args) => {
                        if self.function(*f)?.captures.len() != args.len() {
                            return Err("wrong Flat closure size".into());
                        }
                        self.atoms(args, &scope)?;
                    }
                    Rhs::EnvField(v, i) => {
                        self.atom(&Atom::Var(v.clone()), &scope)?;
                        if !self.environment.as_ref().is_some_and(
                            |(e, n)| e == v && (*i as usize) < *n,
                        ) {
                            return Err(
                                "invalid Flat environment load".into()
                            );
                        }
                    }
                    Rhs::PatchClosure(a, _, value) => {
                        self.atom(a, &scope)?;
                        self.atom(value, &scope)?;
                    }
                }
                self.bind(v)?;
                scope.insert(v.clone());
                self.body(b, scope, joins)
            }
            Flat::Join(j, params, b, r) => {
                self.bind(j)?;
                let mut inner = scope.clone();
                for p in params {
                    self.bind(p)?;
                    inner.insert(p.clone());
                }
                self.body(b, inner, joins.clone())?;
                joins.insert(j.clone(), params.len());
                self.body(r, scope, joins)
            }
            Flat::Case(a, alts) => {
                self.atom(a, &scope)?;
                for (_, ps, b) in alts {
                    let mut inner = scope.clone();
                    for p in ps {
                        self.bind(p)?;
                        inner.insert(p.clone());
                    }
                    self.body(b, inner, joins.clone())?;
                }
                Ok(())
            }
        }
    }
}
/// Check each lifted function in isolation: there are no implicit free variables.
pub fn lint(program: &Program) -> Result<(), String> {
    let globals = program
        .globals
        .iter()
        .map(|g| g.name.clone())
        .collect::<BTreeSet<_>>();
    if globals.len() != program.globals.len() {
        return Err("duplicate Flat global".into());
    }
    let mut c = Check {
        program,
        environment: None,
        globals,
        seen: BTreeSet::new(),
    };
    for (i, f) in program.funcs.iter().enumerate() {
        if f.name.0 as usize != i || f.params.is_empty() {
            return Err("invalid Flat function table".into());
        }
        c.seen.clear();
        c.environment = f.env.clone().map(|e| (e, f.captures.len()));
        let mut scope = BTreeSet::new();
        for p in f.env.iter().chain(&f.params) {
            c.bind(p)?;
            scope.insert(p.clone());
        }
        c.body(&f.body, scope, BTreeMap::new())?;
    }
    c.environment = None;
    for g in &program.globals {
        c.seen.clear();
        if let Some(id) = g.function {
            let f = c.function(id)?;
            if f.env.is_some() || !f.captures.is_empty() {
                return Err("global function has captures".into());
            }
        }
        c.body(&g.body, BTreeSet::new(), BTreeMap::new())?;
    }
    Ok(())
}
