//! First-order code: closures contain function identities and explicit capture slots.
pub use crate::anf::{Atom, Var};
use crate::{
    ast::Op,
    core::{self, AltCon},
    data::ConId,
    source::Span,
};
use std::rc::Rc;
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// An index into this program's lifted function table.
pub struct FuncId(pub u32);
pub type Body = Rc<Flat>;
#[derive(Clone, Debug)]
/// Only lifted local lambdas receive an environment parameter.
pub struct Func {
    pub name: FuncId,
    pub source: Var,
    pub env: Option<Var>,
    pub params: Vec<Var>,
    pub body: Body,
    pub captures: Vec<Var>,
}
#[derive(Clone, Debug)]
/// Function globals are installed before ordered strict value initializers run.
pub struct GlobalInit {
    pub name: Var,
    pub function: Option<FuncId>,
    pub body: Body,
}
#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Vec<Func>,
    pub globals: Vec<GlobalInit>,
    pub metadata: core::Program,
    pub reps: crate::rep::Table,
}
#[derive(Clone, Debug)]
/// PatchClosure is a compiler-only operation used after all letrec allocations.
pub enum Rhs {
    Atom(Atom),
    CallKnown(FuncId, Option<Atom>, Vec<Atom>),
    CallUnknown(Atom, Vec<Atom>),
    AllocCon(ConId, Vec<Atom>),
    AllocClosure(FuncId, Vec<Atom>),
    EnvField(Var, u32),
    Field(Atom, u32),
    Prim(Op, Vec<Atom>, Span),
    PatchClosure(Atom, u32, Atom),
}
#[derive(Clone, Debug)]
/// Control transfers are explicit and joins remain local to each function.
pub enum Flat {
    Let(Var, Rhs, Body),
    Case(Atom, Vec<(AltCon, Vec<Var>, Body)>),
    Join(Var, Vec<Var>, Body, Body),
    Jump(Var, Vec<Atom>),
    Ret(Atom),
    TailCallKnown(FuncId, Option<Atom>, Vec<Atom>),
    TailCallUnknown(Atom, Vec<Atom>),
    Fail(Span),
}
impl std::fmt::Display for FuncId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "f{}", self.0)
    }
}
fn list<T: std::fmt::Display>(xs: &[T]) -> String {
    xs.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}
impl std::fmt::Display for Rhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(a) => write!(f, "{a}"),
            Self::CallKnown(id, env, args) => write!(
                f,
                "call {id}[{}]({})",
                env.as_ref().map(ToString::to_string).unwrap_or_default(),
                list(args)
            ),
            Self::CallUnknown(a, args) => {
                write!(f, "apply {a}({})", list(args))
            }
            Self::AllocCon(c, args) => {
                write!(f, "{}({})", c.name, list(args))
            }
            Self::AllocClosure(id, args) => {
                write!(f, "closure {id}[{}]", list(args))
            }
            Self::EnvField(v, i) => write!(f, "env {v}[{i}]"),
            Self::Field(a, i) => write!(f, "field {a}[{i}]"),
            Self::Prim(p, args, _) => {
                write!(f, "{}({})", p.text(), list(args))
            }
            Self::PatchClosure(a, i, v) => {
                write!(f, "patch {a}[{i}] = {v}")
            }
        }
    }
}
impl std::fmt::Display for Flat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        enum Visit<'a> {
            Body(&'a Flat),
            Text(String),
        }
        let mut work = vec![Visit::Body(self)];
        while let Some(next) = work.pop() {
            let Visit::Body(node) = next else {
                if let Visit::Text(text) = next {
                    f.write_str(&text)?;
                }
                continue;
            };
            match node {
                Self::Let(v, r, b) => {
                    writeln!(f, "let {v} = {r};")?;
                    work.push(Visit::Body(b));
                }
                Self::Join(j, ps, b, r) => {
                    write!(f, "join {j}({}) = {{ ", list(ps))?;
                    work.extend([
                        Visit::Body(r),
                        Visit::Text(" };\n".into()),
                        Visit::Body(b),
                    ]);
                }
                Self::Jump(j, args) => {
                    write!(f, "jump {j}({})", list(args))?
                }
                Self::Ret(a) => write!(f, "return {a}")?,
                Self::TailCallKnown(id, env, args) => write!(
                    f,
                    "tail {id}[{}]({})",
                    env.as_ref()
                        .map(ToString::to_string)
                        .unwrap_or_default(),
                    list(args)
                )?,
                Self::TailCallUnknown(a, args) => {
                    write!(f, "tail-apply {a}({})", list(args))?
                }
                Self::Fail(s) => write!(f, "fail {s:?}")?,
                Self::Case(a, alts) => {
                    writeln!(f, "case {a} {{")?;
                    work.push(Visit::Text("}".into()));
                    for (c, ps, b) in alts.iter().rev() {
                        let head = match c {
                            AltCon::Con(c) => c.name.clone(),
                            AltCon::Lit(l) => l.to_string(),
                            AltCon::Default => "_".into(),
                        };
                        work.extend([
                            Visit::Text(" }\n".into()),
                            Visit::Body(b),
                            Visit::Text(format!(
                                "{head}({}) -> {{ ",
                                list(ps)
                            )),
                        ]);
                    }
                }
            }
        }
        Ok(())
    }
}

/// Drain owned IR edges explicitly: even Rc destruction can otherwise recurse deeply.
impl Drop for Program {
    fn drop(&mut self) {
        let mut work = self
            .funcs
            .drain(..)
            .map(|f| f.body)
            .chain(self.globals.drain(..).map(|g| g.body))
            .collect::<Vec<_>>();
        while let Some(body) = work.pop() {
            let Ok(node) = Rc::try_unwrap(body) else {
                continue;
            };
            match node {
                Flat::Let(_, _, rest) => work.push(rest),
                Flat::Join(_, _, body, rest) => work.extend([body, rest]),
                Flat::Case(_, alts) => {
                    work.extend(alts.into_iter().map(|(_, _, b)| b))
                }
                _ => {}
            }
        }
    }
}
