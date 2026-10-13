// adapted from sdiehl/typechecker-zoo/algorithm-w/src/infer.rs
//! Algorithm W threads substitutions explicitly; schemes remove bound variables from substitutions.
use crate::{
    ast::{Expr, Kind, Op},
    diagnostic::Diagnostic,
    types::{self, Scheme, Type},
};
use std::collections::{BTreeMap, BTreeSet};

type Env = BTreeMap<String, Scheme>;

#[derive(Default, Clone)]
struct Subst(BTreeMap<u32, Type>);

// ANCHOR: substitution
impl Subst {
    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(n) => {
                self.0.get(n).map_or_else(|| ty.clone(), |t| self.apply(t))
            }
            Type::Arrow(a, b) => Type::arrow(self.apply(a), self.apply(b)),
            other => other.clone(),
        }
    }
    /// self after earlier: every earlier range is rewritten by the newer substitution.
    fn compose(&self, earlier: &Self) -> Self {
        let mut result: BTreeMap<_, _> =
            earlier.0.iter().map(|(n, t)| (*n, self.apply(t))).collect();
        for (n, t) in &self.0 {
            result.entry(*n).or_insert_with(|| t.clone());
        }
        Self(result)
    }
    fn environment(&self, env: &Env) -> Env {
        env.iter()
            .map(|(name, scheme)| {
                let mut filtered = self.clone();
                for n in &scheme.variables {
                    filtered.0.remove(n);
                }
                (
                    name.clone(),
                    Scheme {
                        variables: scheme.variables.clone(),
                        ty: filtered.apply(&scheme.ty),
                    },
                )
            })
            .collect()
    }
}
// ANCHOR_END: substitution

fn free(ty: &Type) -> BTreeSet<u32> {
    match ty {
        Type::Var(n) => BTreeSet::from([*n]),
        Type::Arrow(a, b) => free(a).union(&free(b)).copied().collect(),
        _ => BTreeSet::new(),
    }
}

fn generalize(env: &Env, ty: Type) -> Scheme {
    let bound: BTreeSet<_> = env
        .values()
        .flat_map(|s| {
            free(&s.ty)
                .difference(&s.variables.iter().copied().collect())
                .copied()
                .collect::<Vec<_>>()
        })
        .collect();
    Scheme {
        variables: free(&ty).difference(&bound).copied().collect(),
        ty,
    }
}

fn unify(a: &Type, b: &Type, expr: &Expr) -> Result<Subst, Diagnostic> {
    match (a, b) {
        _ if a == b => Ok(Subst::default()),
        (Type::Var(n), t) | (t, Type::Var(n)) => {
            if free(t).contains(n) {
                Err(types::occurs(expr))
            } else {
                Ok(Subst(BTreeMap::from([(*n, t.clone())])))
            }
        }
        (Type::Arrow(a, b), Type::Arrow(c, d)) => {
            let first = unify(a, c, expr)?;
            let second = unify(&first.apply(b), &first.apply(d), expr)?;
            Ok(second.compose(&first))
        }
        _ => Err(types::mismatch(expr, a, b)),
    }
}

#[derive(Default)]
struct Infer {
    next: u32,
}

impl Infer {
    fn fresh(&mut self) -> Type {
        let n = self.next;
        self.next += 1;
        Type::Var(n)
    }
    fn instantiate(&mut self, s: &Scheme) -> Type {
        Subst(s.variables.iter().map(|n| (*n, self.fresh())).collect())
            .apply(&s.ty)
    }
    fn infer(
        &mut self,
        env: &Env,
        expr: &Expr,
    ) -> Result<(Subst, Type), Diagnostic> {
        let empty = Subst::default;
        match &expr.kind {
            Kind::Int(_) => Ok((empty(), Type::Int)),
            Kind::Bool(_) => Ok((empty(), Type::Bool)),
            Kind::Var(n) => {
                let scheme = env.get(n).ok_or_else(|| {
                    Diagnostic::new(
                        "E0101",
                        format!("unbound variable {n}"),
                        expr.span.clone(),
                    )
                })?;
                Ok((empty(), self.instantiate(scheme)))
            }
            Kind::Lam(n, body) => {
                let a = self.fresh();
                let mut local = env.clone();
                local.insert(n.clone(), Scheme::mono(a.clone()));
                let (s, b) = self.infer(&local, body)?;
                Ok((s.clone(), Type::arrow(s.apply(&a), b)))
            }
            // ANCHOR: application
            Kind::App(f, a) => {
                let (s1, tf) = self.infer(env, f)?;
                let (s2, ta) = self.infer(&s1.environment(env), a)?;
                let result = self.fresh();
                let s3 = unify(
                    &s2.apply(&tf),
                    &Type::arrow(ta, result.clone()),
                    expr,
                )?;
                Ok((s3.compose(&s2.compose(&s1)), s3.apply(&result)))
            }
            // ANCHOR_END: application
            // ANCHOR: let
            Kind::Let(n, value, body) => {
                let (s1, t) = self.infer(env, value)?;
                let mut local = s1.environment(env);
                local.insert(n.clone(), generalize(&local, s1.apply(&t)));
                let (s2, result) = self.infer(&local, body)?;
                Ok((s2.compose(&s1), result))
            }
            // ANCHOR_END: let
            Kind::If(c, a, b) => {
                let (s1, t) = self.infer(env, c)?;
                let s2 = unify(&t, &Type::Bool, c)?.compose(&s1);
                let (s3, ta) = self.infer(&s2.environment(env), a)?;
                let s3 = s3.compose(&s2);
                let (s4, tb) = self.infer(&s3.environment(env), b)?;
                let s5 = unify(&s4.apply(&ta), &tb, b)?;
                Ok((s5.compose(&s4.compose(&s3)), s5.apply(&tb)))
            }
            Kind::Binary(op, a, b) => {
                let (s1, ta) = self.infer(env, a)?;
                let s2 = unify(&ta, &Type::Int, a)?.compose(&s1);
                let (s3, tb) = self.infer(&s2.environment(env), b)?;
                let s4 = unify(&tb, &Type::Int, b)?;
                let result = match op {
                    Op::Eq | Op::Lt | Op::Le => Type::Bool,
                    _ => Type::Int,
                };
                Ok((s4.compose(&s3.compose(&s2)), result))
            }
            Kind::Neg(a) => {
                let (s, t) = self.infer(env, a)?;
                Ok((unify(&t, &Type::Int, a)?.compose(&s), Type::Int))
            }
            Kind::Fix(f) => {
                let (s, t) = self.infer(env, f)?;
                let a = self.fresh();
                let solved =
                    unify(&t, &Type::arrow(a.clone(), a.clone()), f)?;
                Ok((solved.compose(&s), solved.apply(&a)))
            }
            Kind::Error => Err(Diagnostic::new(
                "E0002",
                "unrecovered syntax error",
                expr.span.clone(),
            )),
        }
    }
}

/// Infer a closed term and return its principal type with canonical variable names.
pub fn check(expr: &Expr) -> Result<Type, Diagnostic> {
    let (s, t) = Infer::default().infer(&Env::new(), expr)?;
    Ok(s.apply(&t).canonical())
}
