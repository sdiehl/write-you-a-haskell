// adapted from sdiehl/typechecker-zoo/system-f/src/typecheck.rs (substitution structure)
//! Closed System F types are separate from mutable inference metavariables.
use std::collections::BTreeSet;

/// All type binders have kind `*` in this chapter; constructor kinds arrive later.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Char,
    String,
    Var(u32),
    Arrow(Box<Type>, Box<Type>),
    Forall(u32, Box<Type>),
}

impl Type {
    pub fn arrow(a: Self, b: Self) -> Self {
        Self::Arrow(Box::new(a), Box::new(b))
    }
    pub fn free(&self) -> BTreeSet<u32> {
        match self {
            Self::Var(v) => BTreeSet::from([*v]),
            Self::Arrow(a, b) => {
                let mut vars = a.free();
                vars.extend(b.free());
                vars
            }
            Self::Forall(v, b) => {
                let mut vars = b.free();
                vars.remove(v);
                vars
            }
            _ => BTreeSet::new(),
        }
    }
    fn largest(&self) -> u32 {
        match self {
            Self::Var(v) => *v,
            Self::Arrow(a, b) => a.largest().max(b.largest()),
            Self::Forall(v, b) => (*v).max(b.largest()),
            _ => 0,
        }
    }
    /// Substitute without capturing a free variable of the replacement.
    // ANCHOR: type-substitution
    pub fn substitute(&self, variable: u32, replacement: &Type) -> Type {
        match self {
            Self::Var(v) if *v == variable => replacement.clone(),
            Self::Arrow(a, b) => Self::arrow(
                a.substitute(variable, replacement),
                b.substitute(variable, replacement),
            ),
            Self::Forall(v, _) if *v == variable => self.clone(),
            Self::Forall(v, body) => {
                let (v, body) = if replacement.free().contains(v) {
                    let fresh = self
                        .largest()
                        .max(replacement.largest())
                        .max(variable)
                        .checked_add(1)
                        .expect("type variable supply exhausted");
                    (fresh, body.substitute(*v, &Self::Var(fresh)))
                } else {
                    (*v, *body.clone())
                };
                Self::Forall(
                    v,
                    Box::new(body.substitute(variable, replacement)),
                )
            }
            _ => self.clone(),
        }
    }
    // ANCHOR_END: type-substitution
    /// Alpha equivalence compares bound identities by their scope, not spelling.
    pub fn equivalent(&self, other: &Self) -> bool {
        fn go(a: &Type, b: &Type, scope: &mut Vec<(u32, u32)>) -> bool {
            match (a, b) {
                (Type::Var(a), Type::Var(b)) => {
                    let left = scope.iter().rposition(|(v, _)| v == a);
                    let right = scope.iter().rposition(|(_, v)| v == b);
                    match (left, right) {
                        (Some(a), Some(b)) => a == b,
                        (None, None) => a == b,
                        _ => false,
                    }
                }
                (Type::Arrow(a, b), Type::Arrow(c, d)) => {
                    go(a, c, scope) && go(b, d, scope)
                }
                (Type::Forall(a, b), Type::Forall(c, d)) => {
                    scope.push((*a, *c));
                    let same = go(b, d, scope);
                    scope.pop();
                    same
                }
                _ => a == b,
            }
        }
        go(self, other, &mut Vec::new())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Char => write!(f, "Char"),
            Self::String => write!(f, "String"),
            Self::Var(v) => write!(f, "t{v}"),
            Self::Forall(v, b) => write!(f, "forall t{v}. {b}"),
            Self::Arrow(a, b) => {
                if matches!(**a, Self::Arrow(..) | Self::Forall(..)) {
                    write!(f, "({a})")?;
                } else {
                    write!(f, "{a}")?;
                }
                write!(f, " -> {b}")
            }
        }
    }
}
