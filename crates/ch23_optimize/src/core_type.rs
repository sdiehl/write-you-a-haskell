// adapted from sdiehl/typechecker-zoo/system-f/src/typecheck.rs (substitution structure)
//! Closed System F types are separate from mutable inference metavariables.
use std::collections::BTreeSet;

/// Core type applications retain kinds in the program's binder table.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Double,
    Int,
    Bool,
    Char,
    String,
    Var(u32),
    Con(crate::data::TyConId, Vec<Type>),
    App(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
    Forall(u32, Box<Type>),
}

impl Type {
    pub fn apply(f: Self, a: Self) -> Self {
        match f {
            Self::Con(id, mut args) => {
                args.push(a);
                if id.name == "(->)" && args.len() == 2 {
                    Self::arrow(args.remove(0), args.remove(0))
                } else {
                    Self::Con(id, args)
                }
            }
            f => Self::App(Box::new(f), Box::new(a)),
        }
    }
    pub fn arrow(a: Self, b: Self) -> Self {
        Self::Arrow(Box::new(a), Box::new(b))
    }
    pub fn free(&self) -> BTreeSet<u32> {
        match self {
            Self::Var(v) => BTreeSet::from([*v]),
            Self::Con(_, args) => {
                args.iter().flat_map(Self::free).collect()
            }
            Self::Arrow(a, b) | Self::App(a, b) => {
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
            Self::Con(_, args) => {
                args.iter().map(Self::largest).max().unwrap_or(0)
            }
            Self::Arrow(a, b) | Self::App(a, b) => {
                a.largest().max(b.largest())
            }
            Self::Forall(v, b) => (*v).max(b.largest()),
            _ => 0,
        }
    }
    /// Substitute without capturing a free variable of the replacement.
    // ANCHOR: type-substitution
    pub fn substitute(&self, variable: u32, replacement: &Type) -> Type {
        match self {
            Self::Var(v) if *v == variable => replacement.clone(),
            Self::Con(id, args) => Self::Con(
                id.clone(),
                args.iter()
                    .map(|a| a.substitute(variable, replacement))
                    .collect(),
            ),
            Self::App(a, b) => Self::apply(
                a.substitute(variable, replacement),
                b.substitute(variable, replacement),
            ),
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
                (Type::Con(a, xs), Type::Con(b, ys)) => {
                    a == b
                        && xs.len() == ys.len()
                        && xs.iter().zip(ys).all(|(x, y)| go(x, y, scope))
                }
                (Type::Arrow(a, b), Type::Arrow(c, d))
                | (Type::App(a, b), Type::App(c, d)) => {
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
            Self::Double => write!(f, "Double"),
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Char => write!(f, "Char"),
            Self::String => write!(f, "String"),
            Self::Var(v) => write!(f, "t{v}"),
            Self::Con(id, args) => {
                if id.name == "[]" && args.len() == 1 {
                    return write!(f, "[{}]", args[0]);
                }
                if matches!(id.name.as_str(), "(,)" | "(,,)")
                    && args.len() == id.name.len() - 1
                {
                    write!(f, "(")?;
                    for (i, t) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        write!(f, "{t}")?;
                    }
                    return write!(f, ")");
                }
                write!(f, "{}", id.name)?;
                for t in args {
                    if matches!(t, Self::Arrow(..) | Self::Forall(..))
                        || matches!(t,Self::Con(_,v) if !v.is_empty())
                    {
                        write!(f, " ({t})")?;
                    } else {
                        write!(f, " {t}")?;
                    }
                }
                Ok(())
            }
            Self::Forall(v, b) => write!(f, "forall t{v}. {b}"),
            Self::App(a, b) => write!(f, "{a} ({b})"),
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

/// A flat, alpha-normalized key protects imported ABIs when source is rechecked.
/// Both traversal and ownership use vectors, without recursively cloning types.
pub fn fingerprint(ty: &Type) -> Vec<String> {
    enum Work<'a> {
        Visit(&'a Type),
        Leave,
    }
    let mut work = vec![Work::Visit(ty)];
    let mut scope = vec![];
    let mut key = vec![];
    while let Some(item) = work.pop() {
        let Work::Visit(ty) = item else {
            scope.pop();
            continue;
        };
        match ty {
            Type::Var(n) => {
                key.push(match scope.iter().rev().position(|v| v == n) {
                    Some(depth) => format!("bound:{depth}"),
                    None => format!("free:{n}"),
                })
            }
            Type::Con(id, args) => {
                key.push(format!("con:{}:{}", id.name, args.len()));
                work.extend(args.iter().rev().map(Work::Visit));
            }
            Type::Arrow(a, b) | Type::App(a, b) => {
                key.push(
                    if matches!(ty, Type::Arrow(..)) {
                        "arrow"
                    } else {
                        "app"
                    }
                    .into(),
                );
                work.push(Work::Visit(b));
                work.push(Work::Visit(a));
            }
            Type::Forall(n, body) => {
                key.push("forall".into());
                scope.push(*n);
                work.push(Work::Leave);
                work.push(Work::Visit(body));
            }
            Type::Int => key.push("Int".into()),
            Type::Double => key.push("Double".into()),
            Type::Bool => key.push("Bool".into()),
            Type::Char => key.push("Char".into()),
            Type::String => key.push("String".into()),
        }
    }
    key
}
