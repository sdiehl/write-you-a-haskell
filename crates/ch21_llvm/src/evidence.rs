//! Freeze class proofs after zonking, while lexical dictionary scopes are explicit.
use crate::{
    classes::Evidence,
    diagnostics::Diagnostic,
    renamed::{BindGroup, Expr, Kind, RowBody},
    typecheck::TypedProgram,
    types::Predicate,
};
use std::collections::BTreeMap;

/// Each wanted at a variable occurrence has an identity independent of its type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
// ANCHOR: evidence-variable
pub struct EvVar {
    pub node: u32,
    pub index: usize,
}
// ANCHOR_END: evidence-variable

/// Record proofs for every source occurrence before any Core is constructed.
pub fn record(
    program: &TypedProgram,
) -> Result<BTreeMap<EvVar, Evidence>, Diagnostic> {
    struct Pass<'a> {
        program: &'a TypedProgram,
        givens: Vec<Predicate>,
        proofs: BTreeMap<EvVar, Evidence>,
    }
    impl Pass<'_> {
        fn group(&mut self, group: &BindGroup) -> Result<(), Diagnostic> {
            for b in &group.binds {
                let size = self.givens.len();
                self.givens
                    .extend(self.program.schemes[&b.id].predicates.clone());
                self.expr(&b.rhs)?;
                self.givens.truncate(size);
            }
            Ok(())
        }
        fn expr(&mut self, e: &Expr) -> Result<(), Diagnostic> {
            match &e.kind {
                Kind::Var(id) => {
                    let mut predicates = self
                        .program
                        .occurrence_predicates
                        .get(&e.node)
                        .cloned()
                        .unwrap_or_default();
                    if predicates.is_empty()
                        && let Some(scheme) = self.program.schemes.get(id)
                    {
                        predicates = scheme.predicates.clone();
                    }
                    for (index, p) in predicates.iter().enumerate() {
                        let proof = self.program.surface.classes.resolve(
                            p,
                            &self.givens,
                            e.span,
                        )?;
                        self.proofs.insert(
                            EvVar {
                                node: e.node,
                                index,
                            },
                            proof,
                        );
                    }
                }
                Kind::Lam(_, e)
                | Kind::Neg(e)
                | Kind::Annotated(e, _)
                | Kind::Project(e, _) => self.expr(e)?,
                Kind::App(a, b) | Kind::Binary(_, a, b) => {
                    let mut work = vec![b.as_ref(), a.as_ref()];
                    while let Some(e) = work.pop() {
                        if let Kind::App(a, b) | Kind::Binary(_, a, b) =
                            &e.kind
                        {
                            work.push(b);
                            work.push(a);
                        } else {
                            self.expr(e)?;
                        }
                    }
                }
                Kind::Let(groups, body) => {
                    for g in groups {
                        self.group(g)?;
                    }
                    self.expr(body)?;
                }
                Kind::Match(scruts, rows) => {
                    for e in scruts {
                        self.expr(e)?;
                    }
                    for row in rows {
                        for g in &row.locals {
                            self.group(g)?;
                        }
                        match &row.body {
                            RowBody::Plain(e) => self.expr(e)?,
                            RowBody::Guarded(gs) => {
                                for (g, e) in gs {
                                    self.expr(g)?;
                                    self.expr(e)?;
                                }
                            }
                        }
                    }
                }
                Kind::Record(_, fields) => {
                    for (_, e) in fields {
                        self.expr(e)?;
                    }
                }
                Kind::Update(e, fields) => {
                    self.expr(e)?;
                    for (_, e) in fields {
                        self.expr(e)?;
                    }
                }
                Kind::If(c, a, b) => {
                    self.expr(c)?;
                    let mut work = vec![b.as_ref(), a.as_ref()];
                    while let Some(e) = work.pop() {
                        if let Kind::App(a, b) | Kind::Binary(_, a, b) =
                            &e.kind
                        {
                            work.push(b);
                            work.push(a);
                        } else {
                            self.expr(e)?;
                        }
                    }
                }
                _ => {}
            }
            Ok(())
        }
    }
    let mut pass = Pass {
        program,
        givens: vec![],
        proofs: BTreeMap::new(),
    };
    for group in &program.surface.groups {
        pass.group(group)?;
    }
    Ok(pass.proofs)
}
