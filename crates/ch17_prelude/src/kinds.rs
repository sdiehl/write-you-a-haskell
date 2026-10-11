// adapted from sdiehl/typechecker-zoo/system-f-omega/src/core.rs (kind/type application judgments)
//! Handwritten kind unification and dependency-ordered declaration checking.
use crate::{
    ast::{DataDecl, TypeAlias, TypeSyntax},
    diagnostics::{Diagnostic, ErrorCode},
    source::Span,
};
use std::collections::{BTreeMap, BTreeSet};
/// Kind metavariables exist only while checking a declaration component.
#[derive(Clone, Debug, PartialEq, Eq)]
// ANCHOR: kind
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
    Meta(u32),
}
// ANCHOR_END: kind
impl Kind {
    pub fn arrow(a: Self, b: Self) -> Self {
        Self::Arrow(Box::new(a), Box::new(b))
    }
    pub fn parameters(&self) -> Vec<Self> {
        let mut k = self;
        let mut result = vec![];
        while let Self::Arrow(a, b) = k {
            result.push(*a.clone());
            k = b;
        }
        result
    }
}
impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Star => write!(f, "*"),
            Self::Meta(n) => write!(f, "k{n}"),
            Self::Arrow(a, b) => {
                if matches!(**a, Self::Arrow(..)) {
                    write!(f, "({a}) -> {b}")
                } else {
                    write!(f, "{a} -> {b}")
                }
            }
        }
    }
}
/// Cells use path compression and an occurs check, independently of type inference.
#[derive(Default)]
pub struct Store {
    solutions: Vec<Option<Kind>>,
}
impl Store {
    pub fn fresh(&mut self) -> Kind {
        let n = self.solutions.len();
        self.solutions.push(None);
        Kind::Meta(n as u32)
    }
    pub fn zonk(&mut self, k: &Kind) -> Kind {
        match k {
            Kind::Meta(n) => {
                if let Some(k) = self.solutions[*n as usize].clone() {
                    let k = self.zonk(&k);
                    self.solutions[*n as usize] = Some(k.clone());
                    k
                } else {
                    k.clone()
                }
            }
            Kind::Arrow(a, b) => Kind::arrow(self.zonk(a), self.zonk(b)),
            Kind::Star => Kind::Star,
        }
    }
    fn occurs(&mut self, n: u32, k: &Kind) -> bool {
        match self.zonk(k) {
            Kind::Meta(m) => n == m,
            Kind::Arrow(a, b) => self.occurs(n, &a) || self.occurs(n, &b),
            _ => false,
        }
    }
    // ANCHOR: kind-unify
    pub fn unify(
        &mut self,
        a: &Kind,
        b: &Kind,
        span: Span,
    ) -> Result<(), Diagnostic> {
        let (a, b) = (self.zonk(a), self.zonk(b));
        match (&a, &b) {
            _ if a == b => Ok(()),
            (Kind::Meta(n), k) | (k, Kind::Meta(n))
                if !self.occurs(*n, k) =>
            {
                self.solutions[*n as usize] = Some(k.clone());
                Ok(())
            }
            (Kind::Arrow(a, b), Kind::Arrow(c, d)) => {
                self.unify(a, c, span)?;
                self.unify(b, d, span)
            }
            _ => Err(Diagnostic::error(
                ErrorCode::Kind,
                format!("cannot unify kind {a} with {b}"),
                span,
            )),
        }
    }
    // ANCHOR_END: kind-unify
    pub fn finish(&mut self, k: &Kind) -> Kind {
        match self.zonk(k) {
            Kind::Meta(n) => {
                self.solutions[n as usize] = Some(Kind::Star);
                Kind::Star
            }
            Kind::Arrow(a, b) => {
                Kind::arrow(self.finish(&a), self.finish(&b))
            }
            Kind::Star => Kind::Star,
        }
    }
    /// Application consumes one arrow in the function's kind.
    // ANCHOR: kind-application
    pub fn infer(
        &mut self,
        syntax: &TypeSyntax,
        heads: &BTreeMap<String, Kind>,
        variables: &mut BTreeMap<String, Kind>,
        implicit: bool,
        span: Span,
    ) -> Result<Kind, Diagnostic> {
        match syntax {
            TypeSyntax::Qualified(_, t) => {
                self.infer(t, heads, variables, implicit, span)
            }
            TypeSyntax::Name(n) => {
                if let Some(k) = variables.get(n).or_else(|| heads.get(n)) {
                    return Ok(k.clone());
                }
                if implicit
                    && (n.starts_with(char::is_lowercase)
                        || n.starts_with('_'))
                {
                    let k = self.fresh();
                    variables.insert(n.clone(), k.clone());
                    return Ok(k);
                }
                Err(Diagnostic::error(
                    ErrorCode::UnboundConstructor,
                    format!("unknown type {n}"),
                    span,
                ))
            }
            TypeSyntax::App(a, b) => {
                let f = self.infer(a, heads, variables, implicit, span)?;
                let a = self.infer(b, heads, variables, implicit, span)?;
                let result = self.fresh();
                self.unify(&f, &Kind::arrow(a, result.clone()), span)?;
                Ok(result)
            }
            TypeSyntax::Arrow(a, b) => {
                let a = self.infer(a, heads, variables, implicit, span)?;
                let b = self.infer(b, heads, variables, implicit, span)?;
                self.unify(&a, &Kind::Star, span)?;
                self.unify(&b, &Kind::Star, span)?;
                Ok(Kind::Star)
            }
        }
    }
}
// ANCHOR_END: kind-application
fn references(ty: &TypeSyntax, out: &mut BTreeSet<String>) {
    match ty {
        TypeSyntax::Qualified(_, t) => references(t, out),
        TypeSyntax::Name(n) => {
            out.insert(n.clone());
        }
        TypeSyntax::App(a, b) | TypeSyntax::Arrow(a, b) => {
            references(a, out);
            references(b, out);
        }
    }
}
pub fn builtins() -> BTreeMap<String, Kind> {
    let mut env = ["Int", "Double", "Bool", "Char", "String"]
        .into_iter()
        .map(|n| (n.into(), Kind::Star))
        .collect::<BTreeMap<_, _>>();
    env.insert("IO".into(), Kind::arrow(Kind::Star, Kind::Star));
    env.insert(
        "(->)".into(),
        Kind::arrow(Kind::Star, Kind::arrow(Kind::Star, Kind::Star)),
    );
    env
}
/// Infer each SCC before defaulting unconstrained parameters in that component.
pub fn declarations(
    data: &[DataDecl],
    aliases: &[TypeAlias],
) -> Result<BTreeMap<String, Kind>, Diagnostic> {
    let names = data
        .iter()
        .map(|d| (&d.name, &d.parameters, d.span))
        .chain(aliases.iter().map(|a| (&a.name, &a.parameters, a.span)))
        .collect::<Vec<_>>();
    let mut store = Store::default();
    let mut heads = builtins();
    let mut params = vec![];
    let mut results = vec![];
    for (name, ps, span) in &names {
        if names.iter().filter(|(n, _, _)| n == name).count() > 1 {
            return Err(Diagnostic::error(
                ErrorCode::Duplicate,
                format!("duplicate type {name}"),
                *span,
            ));
        }
        let mut variables = BTreeMap::new();
        for p in *ps {
            if variables.insert(p.clone(), store.fresh()).is_some() {
                return Err(Diagnostic::error(
                    ErrorCode::Duplicate,
                    format!("duplicate type parameter {p}"),
                    *span,
                ));
            }
        }
        let result = if params.len() < data.len() {
            Kind::Star
        } else {
            store.fresh()
        };
        results.push(result.clone());
        let kind = ps
            .iter()
            .rev()
            .fold(result, |k, p| Kind::arrow(variables[p].clone(), k));
        heads.insert((*name).clone(), kind);
        params.push(variables);
    }
    let edges = names
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let mut refs = BTreeSet::new();
            if i < data.len() {
                for c in &data[i].constructors {
                    for t in &c.fields {
                        references(t, &mut refs);
                    }
                }
            } else {
                references(&aliases[i - data.len()].body, &mut refs);
            }
            names
                .iter()
                .enumerate()
                .filter_map(|(i, (n, _, _))| refs.contains(*n).then_some(i))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    for component in crate::scc::components(&edges) {
        for &i in &component {
            let fields = if i < data.len() {
                data[i]
                    .constructors
                    .iter()
                    .flat_map(|c| c.fields.iter())
                    .collect::<Vec<_>>()
            } else {
                vec![&aliases[i - data.len()].body]
            };
            for t in fields {
                let k = store.infer(
                    t,
                    &heads,
                    &mut params[i],
                    false,
                    names[i].2,
                )?;
                store.unify(&k, &results[i], names[i].2)?;
            }
        }
        for i in component {
            let k = store.finish(&heads[names[i].0]);
            heads.insert(names[i].0.clone(), k);
        }
    }
    Ok(heads)
}
/// Expand fully applied synonyms before kind checking. A stack detects cycles.
pub fn expand(
    ty: &TypeSyntax,
    aliases: &[TypeAlias],
    span: Span,
) -> Result<TypeSyntax, Diagnostic> {
    fn go(
        ty: &TypeSyntax,
        aliases: &[TypeAlias],
        stack: &mut Vec<String>,
        span: Span,
    ) -> Result<TypeSyntax, Diagnostic> {
        if let TypeSyntax::Qualified(ps, t) = ty {
            return Ok(TypeSyntax::Qualified(
                ps.clone(),
                Box::new(go(t, aliases, stack, span)?),
            ));
        }
        if let TypeSyntax::Arrow(a, b) = ty {
            return Ok(TypeSyntax::Arrow(
                Box::new(go(a, aliases, stack, span)?),
                Box::new(go(b, aliases, stack, span)?),
            ));
        }
        let mut head = ty;
        let mut args = vec![];
        while let TypeSyntax::App(f, a) = head {
            args.push(&**a);
            head = f;
        }
        args.reverse();
        // Expand arguments before entering this synonym, permitting Id (Id Int).
        let mut args = args
            .into_iter()
            .map(|a| go(a, aliases, stack, span))
            .collect::<Result<Vec<_>, _>>()?;
        if let TypeSyntax::Name(n) = head
            && let Some(alias) = aliases.iter().find(|a| &a.name == n)
        {
            if args.len() < alias.parameters.len() {
                return Err(Diagnostic::error(
                    ErrorCode::PartialSynonym,
                    format!("type synonym {n} must be fully applied"),
                    span,
                ));
            }
            if stack.contains(n) {
                return Err(Diagnostic::error(
                    ErrorCode::SynonymCycle,
                    format!("cyclic type synonym {n}"),
                    span,
                ));
            }
            let rest = args.split_off(alias.parameters.len());
            let mapping = alias
                .parameters
                .iter()
                .cloned()
                .zip(args)
                .collect::<BTreeMap<_, _>>();
            fn replace(
                t: &TypeSyntax,
                map: &BTreeMap<String, TypeSyntax>,
            ) -> TypeSyntax {
                match t {
                    TypeSyntax::Qualified(ps, t) => TypeSyntax::Qualified(
                        ps.clone(),
                        Box::new(replace(t, map)),
                    ),
                    TypeSyntax::Name(n) => {
                        map.get(n).cloned().unwrap_or_else(|| t.clone())
                    }
                    TypeSyntax::App(a, b) => TypeSyntax::App(
                        Box::new(replace(a, map)),
                        Box::new(replace(b, map)),
                    ),
                    TypeSyntax::Arrow(a, b) => TypeSyntax::Arrow(
                        Box::new(replace(a, map)),
                        Box::new(replace(b, map)),
                    ),
                }
            }
            stack.push(n.clone());
            let body =
                go(&replace(&alias.body, &mapping), aliases, stack, span)?;
            stack.pop();
            return Ok(rest.into_iter().fold(body, |f, a| {
                TypeSyntax::App(Box::new(f), Box::new(a))
            }));
        }
        Ok(args.into_iter().fold(head.clone(), |f, a| {
            TypeSyntax::App(Box::new(f), Box::new(a))
        }))
    }
    go(ty, aliases, &mut vec![], span)
}
