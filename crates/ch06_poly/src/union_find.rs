//! Level-based HM. Metavariables are mutable cells; zonking compresses their solution paths.
use crate::{
    ast::{Expr, Kind, Op},
    diagnostic::Diagnostic,
    types::{self, Scheme, Type},
};
use std::collections::{BTreeMap, BTreeSet};

// ANCHOR: store
#[derive(Clone)]
struct Meta {
    level: u32,
    solution: Option<Type>,
}

#[derive(Default)]
struct Infer {
    level: u32,
    metas: Vec<Meta>,
}

impl Infer {
    fn fresh(&mut self) -> Type {
        let n = self.metas.len() as u32;
        self.metas.push(Meta {
            level: self.level,
            solution: None,
        });
        Type::Var(n)
    }
    fn zonk(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Var(n) => {
                match self.metas[*n as usize].solution.clone() {
                    None => ty.clone(),
                    Some(t) => {
                        let t = self.zonk(&t);
                        self.metas[*n as usize].solution = Some(t.clone());
                        t
                    }
                }
            }
            Type::Arrow(a, b) => Type::arrow(self.zonk(a), self.zonk(b)),
            other => other.clone(),
        }
    }
    // ANCHOR_END: store
    // ANCHOR: levels
    /// One traversal rejects cycles and prevents younger variables escaping their scope.
    fn occurs_and_lower(
        &mut self,
        n: u32,
        level: u32,
        ty: &Type,
        expr: &Expr,
    ) -> Result<(), Diagnostic> {
        match self.zonk(ty) {
            Type::Var(m) if m == n => Err(types::occurs(expr)),
            Type::Var(m) => {
                self.metas[m as usize].level =
                    self.metas[m as usize].level.min(level);
                Ok(())
            }
            Type::Arrow(a, b) => {
                self.occurs_and_lower(n, level, &a, expr)?;
                self.occurs_and_lower(n, level, &b, expr)
            }
            _ => Ok(()),
        }
    }
    fn unify(
        &mut self,
        a: &Type,
        b: &Type,
        expr: &Expr,
    ) -> Result<(), Diagnostic> {
        let (a, b) = (self.zonk(a), self.zonk(b));
        match (&a, &b) {
            _ if a == b => Ok(()),
            (Type::Var(n), t) | (t, Type::Var(n)) => {
                self.occurs_and_lower(
                    *n,
                    self.metas[*n as usize].level,
                    t,
                    expr,
                )?;
                self.metas[*n as usize].solution = Some(t.clone());
                Ok(())
            }
            (Type::Arrow(a, b), Type::Arrow(c, d)) => {
                self.unify(a, c, expr)?;
                self.unify(b, d, expr)
            }
            _ => Err(types::mismatch(expr, &a, &b)),
        }
    }
    // ANCHOR_END: levels
    fn generalize(&mut self, ty: Type) -> Scheme {
        fn visit(
            ty: &Type,
            metas: &[Meta],
            level: u32,
            vars: &mut BTreeSet<u32>,
        ) {
            match ty {
                Type::Var(n) if metas[*n as usize].level > level => {
                    vars.insert(*n);
                }
                Type::Arrow(a, b) => {
                    visit(a, metas, level, vars);
                    visit(b, metas, level, vars);
                }
                _ => {}
            }
        }
        let ty = self.zonk(&ty);
        let mut vars = BTreeSet::new();
        visit(&ty, &self.metas, self.level, &mut vars);
        Scheme {
            variables: vars.into_iter().collect(),
            ty,
        }
    }
    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        fn replace(ty: &Type, mapping: &BTreeMap<u32, Type>) -> Type {
            match ty {
                Type::Var(n) => {
                    mapping.get(n).cloned().unwrap_or_else(|| ty.clone())
                }
                Type::Arrow(a, b) => {
                    Type::arrow(replace(a, mapping), replace(b, mapping))
                }
                other => other.clone(),
            }
        }
        let mapping = scheme
            .variables
            .iter()
            .map(|n| (*n, self.fresh()))
            .collect();
        replace(&scheme.ty, &mapping)
    }
    fn infer(
        &mut self,
        env: &mut Vec<(String, Scheme)>,
        expr: &Expr,
    ) -> Result<Type, Diagnostic> {
        match &expr.kind {
            Kind::Int(_) => Ok(Type::Int),
            Kind::Bool(_) => Ok(Type::Bool),
            Kind::Var(n) => {
                let scheme = env
                    .iter()
                    .rev()
                    .find(|(name, _)| name == n)
                    .map(|(_, s)| s.clone())
                    .ok_or_else(|| {
                        Diagnostic::new(
                            "E0101",
                            format!("unbound variable {n}"),
                            expr.span.clone(),
                        )
                    })?;
                Ok(self.instantiate(&scheme))
            }
            Kind::Lam(n, body) => {
                let a = self.fresh();
                env.push((n.clone(), Scheme::mono(a.clone())));
                let result = self.infer(env, body);
                env.pop();
                Ok(Type::arrow(a, result?))
            }
            Kind::App(f, a) => {
                let tf = self.infer(env, f)?;
                let ta = self.infer(env, a)?;
                let result = self.fresh();
                self.unify(&tf, &Type::arrow(ta, result.clone()), expr)?;
                Ok(result)
            }
            // ANCHOR: let
            Kind::Let(n, value, body) => {
                self.level += 1;
                let result = self.infer(env, value);
                self.level -= 1;
                let scheme = self.generalize(result?);
                env.push((n.clone(), scheme));
                let result = self.infer(env, body);
                env.pop();
                result
            }
            // ANCHOR_END: let
            Kind::If(c, a, b) => {
                let tc = self.infer(env, c)?;
                self.unify(&tc, &Type::Bool, c)?;
                let ta = self.infer(env, a)?;
                let tb = self.infer(env, b)?;
                self.unify(&ta, &tb, b)?;
                Ok(ta)
            }
            Kind::Binary(op, a, b) => {
                let ta = self.infer(env, a)?;
                self.unify(&ta, &Type::Int, a)?;
                let tb = self.infer(env, b)?;
                self.unify(&tb, &Type::Int, b)?;
                Ok(match op {
                    Op::Eq | Op::Lt | Op::Le => Type::Bool,
                    _ => Type::Int,
                })
            }
            Kind::Neg(a) => {
                let t = self.infer(env, a)?;
                self.unify(&t, &Type::Int, a)?;
                Ok(Type::Int)
            }
            Kind::Fix(f) => {
                let tf = self.infer(env, f)?;
                let a = self.fresh();
                self.unify(&tf, &Type::arrow(a.clone(), a.clone()), f)?;
                Ok(a)
            }
            Kind::Error => Err(Diagnostic::new(
                "E0002",
                "unrecovered syntax error",
                expr.span.clone(),
            )),
        }
    }
}

/// Infer using mutable union-find solutions and generalization by levels.
pub fn check(expr: &Expr) -> Result<Type, Diagnostic> {
    let mut infer = Infer {
        level: 1,
        metas: vec![],
    };
    let ty = infer.infer(&mut vec![], expr)?;
    Ok(infer.zonk(&ty).canonical())
}
