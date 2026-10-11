//! System F Core keeps typed constructors and nested matches separate from source sugar.
pub use crate::core_type::Type;
use crate::{ast::Op, source::Span};
/// A Core identifier carries its independently checkable type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id {
    pub name: crate::names::Id,
    pub ty: Type,
}
impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.name, self.ty)
    }
}
// ANCHOR: core
#[derive(Clone, Debug, PartialEq, Eq)]
/// Immediate source values; strings are primitive UTF-8 values, not character lists.
pub enum Lit {
    Double(u64),
    Int(i64),
    Char(char),
    String(String),
}
#[derive(Clone, Debug)]
/// System F Core makes type abstraction and instantiation explicit.
pub enum Expr {
    Var(Id),
    Global(Id),
    TyLam(u32, Box<Expr>),
    TyApp(Box<Expr>, Type),
    Lit(Lit),
    Lam(Id, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(BindGroup, Box<Expr>),
    Con(crate::data::ConId, Vec<Type>, Vec<Expr>),
    Match(Match),
    PatternFail(Span, Type),
    Prim(Op, Vec<Type>, Vec<Expr>, Span),
}
#[derive(Clone, Debug)]
/// A recursive binding is guaranteed to have a lambda right-hand side.
pub struct Bind {
    pub name: Id,
    pub rhs: Box<Expr>,
}
#[derive(Clone, Debug)]
/// An ordered component; recursive members are in scope in every right-hand side.
pub struct BindGroup {
    pub recursive: bool,
    pub binds: Vec<Bind>,
}
#[derive(Clone, Debug)]
/// Globals are initialized in dependency order, with explicit recursive components.
pub struct Program {
    pub groups: Vec<BindGroup>,
    pub main: Id,
    pub data_types: Vec<(crate::data::TyConId, crate::kinds::Kind)>,
    pub type_kinds: std::collections::BTreeMap<u32, crate::kinds::Kind>,
    pub constructors: Vec<ConDecl>,
    pub locations: Vec<(Span, String)>,
}
// ANCHOR_END: core
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(n) => write!(f, "{}", n.name),
            Self::Global(n) => write!(f, "@{}", n.name),
            Self::TyLam(v, b) => write!(f, "(type-lambda t{v} {b})"),
            Self::TyApp(a, t) => write!(f, "(type-app {a} {t})"),
            Self::Lit(l) => write!(f, "{l}"),
            Self::Lam(n, b) => write!(f, "(lambda {n} {b})"),
            Self::App(a, b) => write!(f, "(app {a} {b})"),
            Self::Let(group, body) => write!(f, "(let {group} in {body})"),
            Self::Con(id, types, args) => {
                write!(f, "(con {}", id.name)?;
                for t in types {
                    write!(f, " [{t}]")?;
                }
                for a in args {
                    write!(f, " {a}")?;
                }
                write!(f, ")")
            }
            Self::Match(m) => {
                write!(f, "(match")?;
                for s in &m.scruts {
                    write!(f, " {}", s.name)?;
                }
                for row in &m.rows {
                    write!(f, " {row}")?;
                }
                write!(f, ")")
            }
            Self::PatternFail(_, ty) => write!(f, "(pattern-fail : {ty})"),
            Self::Prim(op, _, args, _) => {
                write!(f, "(prim {}", op.text())?;
                for a in args {
                    write!(f, " {a}")?;
                }
                write!(f, ")")
            }
        }
    }
}
impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Double(bits) => write!(
                f,
                "{}",
                crate::primitives::show_double(f64::from_bits(*bits))
            ),
            Self::Int(n) => write!(f, "{n}"),
            Self::Char(c) => {
                write!(f, "{}", crate::ast::quote(&c.to_string(), '\''))
            }
            Self::String(s) => write!(f, "{}", crate::ast::quote(s, '"')),
        }
    }
}
impl std::fmt::Display for BindGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", if self.recursive { "rec" } else { "nonrec" })?;
        for b in &self.binds {
            write!(f, " {} = {};", b.name, b.rhs)?;
        }
        write!(f, " }}")
    }
}
/// Independently typecheck System F Core after each Core-producing pass.
pub fn lint(program: &Program) -> Result<(), String> {
    crate::core_lint::lint(program)
}

impl Id {
    pub fn unique(&self) -> u32 {
        self.name.name.unique
    }
}

/// Constructor typing is immutable and contains no inference metavariables.
#[derive(Clone, Debug)]
pub struct ConDecl {
    pub labels: Option<Vec<String>>,
    pub id: crate::data::ConId,
    pub tag: u32,
    pub parameters: Vec<u32>,
    pub fields: Vec<Type>,
    pub result: Type,
}
// ANCHOR: nested-match
/// Nested patterns remain until the decision-tree compiler in Chapter 16.
#[derive(Clone, Debug)]
pub enum Pat {
    Var(Id),
    Wild,
    As(Id, Box<Pat>),
    Con(crate::data::ConId, Vec<Pat>),
    Lit(Lit),
}
/// A guarded row falls through when all its Boolean tests fail.
#[derive(Clone, Debug)]
pub enum RowBody {
    Plain(Box<Expr>),
    Guarded(Vec<(Expr, Expr)>),
}
/// Row-local declarations run after patterns match and are shared by the guards.
#[derive(Clone, Debug)]
pub struct Row {
    pub pats: Vec<Pat>,
    pub locals: Vec<BindGroup>,
    pub body: RowBody,
    pub span: Span,
}
/// Scrutinees name strict preceding lets; the match records its result type and failure site.
#[derive(Clone, Debug)]
pub struct Match {
    pub scruts: Vec<Id>,
    pub rows: Vec<Row>,
    pub result: Type,
    pub span: Span,
}
// ANCHOR_END: nested-match
impl std::fmt::Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(id) => write!(f, "{}", id.name),
            Self::Wild => write!(f, "_"),
            Self::As(id, p) => write!(f, "{}@{p}", id.name),
            Self::Lit(l) => write!(f, "{l}"),
            Self::Con(id, ps) => {
                write!(f, "({}", id.name)?;
                for p in ps {
                    write!(f, " {p}")?;
                }
                write!(f, ")")
            }
        }
    }
}
impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;
        for p in &self.pats {
            write!(f, "{p} ")?;
        }
        for local in &self.locals {
            write!(f, "{local} ")?;
        }
        match &self.body {
            RowBody::Plain(e) => write!(f, "-> {e}")?,
            RowBody::Guarded(gs) => {
                for (g, e) in gs {
                    write!(f, "| {g} -> {e} ")?;
                }
            }
        }
        write!(f, " ]")
    }
}

/// Type abstractions erase; a constrained value is an ordinary dictionary function.
pub fn is_function(mut e: &Expr) -> bool {
    while let Expr::TyLam(_, body) = e {
        e = body;
    }
    matches!(e, Expr::Lam(..))
}
/// Dictionary calls introduce global dependencies absent from the source graph.
pub fn order(groups: Vec<BindGroup>) -> Vec<BindGroup> {
    fn refs(e: &Expr, out: &mut std::collections::BTreeSet<u32>) {
        match e {
            Expr::Global(id) => {
                out.insert(id.unique());
            }
            Expr::Lam(_, b) | Expr::TyLam(_, b) | Expr::TyApp(b, _) => {
                refs(b, out)
            }
            Expr::App(a, b) => {
                refs(a, out);
                refs(b, out);
            }
            Expr::Let(g, b) => {
                for bind in &g.binds {
                    refs(&bind.rhs, out);
                }
                refs(b, out);
            }
            Expr::Con(_, _, args) | Expr::Prim(_, _, args, _) => {
                for a in args {
                    refs(a, out);
                }
            }
            Expr::Match(m) => {
                for row in &m.rows {
                    for g in &row.locals {
                        for b in &g.binds {
                            refs(&b.rhs, out);
                        }
                    }
                    match &row.body {
                        RowBody::Plain(e) => refs(e, out),
                        RowBody::Guarded(gs) => {
                            for (g, e) in gs {
                                refs(g, out);
                                refs(e, out);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    let binds =
        groups.into_iter().flat_map(|g| g.binds).collect::<Vec<_>>();
    let indices = binds
        .iter()
        .enumerate()
        .map(|(i, b)| (b.name.unique(), i))
        .collect::<std::collections::BTreeMap<_, _>>();
    let edges = binds
        .iter()
        .map(|b| {
            let mut names = std::collections::BTreeSet::new();
            refs(&b.rhs, &mut names);
            names
                .into_iter()
                .filter_map(|n| indices.get(&n).copied())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut binds = binds.into_iter().map(Some).collect::<Vec<_>>();
    crate::scc::components(&edges)
        .into_iter()
        .map(|xs| BindGroup {
            recursive: (xs.len() > 1 || edges[xs[0]].contains(&xs[0]))
                && xs
                    .iter()
                    .all(|i| is_function(&binds[*i].as_ref().unwrap().rhs)),
            binds: xs
                .into_iter()
                .map(|i| binds[i].take().unwrap())
                .collect(),
        })
        .collect()
}
