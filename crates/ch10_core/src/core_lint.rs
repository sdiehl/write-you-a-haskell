// adapted from sdiehl/typechecker-zoo/system-f/src/typecheck.rs (checking judgments)
//! Syntax-directed System F checking, with no call to the surface inferencer.
use crate::{
    ast::Op,
    core::{BindGroup, Expr, Id, Lit, Program, Type},
};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Default)]
struct Checker {
    terms: BTreeMap<u32, Type>,
    types: BTreeSet<u32>,
    seen_terms: BTreeSet<u32>,
    seen_types: BTreeSet<u32>,
    globals: BTreeSet<u32>,
}

impl Checker {
    fn well_formed(&self, ty: &Type) -> Result<(), String> {
        if let Some(v) = ty.free().difference(&self.types).next() {
            Err(format!("unbound Core type variable t{v}"))
        } else {
            Ok(())
        }
    }
    fn binder(&mut self, id: &Id) -> Result<(), String> {
        self.well_formed(&id.ty)?;
        if !self.seen_terms.insert(id.unique()) {
            return Err(format!("duplicate Core binder {}", id.name));
        }
        Ok(())
    }
    fn expect(&self, actual: &Type, expected: &Type) -> Result<(), String> {
        if actual.equivalent(expected) {
            Ok(())
        } else {
            Err(format!(
                "Core type mismatch: expected {expected}, found {actual}"
            ))
        }
    }
    fn expression(&mut self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::Lit(l) => Ok(match l {
                Lit::Int(_) => Type::Int,
                Lit::Bool(_) => Type::Bool,
                Lit::Char(_) => Type::Char,
                Lit::String(_) => Type::String,
            }),
            Expr::Var(id) | Expr::Global(id) => {
                if matches!(expr, Expr::Global(_))
                    != self.globals.contains(&id.unique())
                {
                    return Err(format!(
                        "Core local/global namespace mismatch for {}",
                        id.name
                    ));
                }
                let ty = self.terms.get(&id.unique()).ok_or_else(|| {
                    format!("unbound Core variable {}", id.name)
                })?;
                self.well_formed(&id.ty)?;
                self.expect(&id.ty, ty)?;
                Ok(ty.clone())
            }
            Expr::Lam(id, body) => {
                self.binder(id)?;
                self.terms.insert(id.unique(), id.ty.clone());
                let body = self.expression(body)?;
                self.terms.remove(&id.unique());
                Ok(Type::arrow(id.ty.clone(), body))
            }
            Expr::App(fun, arg) => {
                let function = self.expression(fun)?;
                let argument = self.expression(arg)?;
                let Type::Arrow(input, output) = function else {
                    return Err(format!(
                        "Core application of non-function type {function}"
                    ));
                };
                self.expect(&argument, &input)?;
                Ok(*output)
            }
            // ANCHOR: type-rules
            Expr::TyLam(variable, body) => {
                if !self.seen_types.insert(*variable) {
                    return Err(format!(
                        "duplicate Core type binder t{variable}"
                    ));
                }
                self.types.insert(*variable);
                let body = self.expression(body)?;
                self.types.remove(variable);
                Ok(Type::Forall(*variable, Box::new(body)))
            }
            Expr::TyApp(fun, argument) => {
                self.well_formed(argument)?;
                let function = self.expression(fun)?;
                let Type::Forall(variable, body) = function else {
                    return Err(format!(
                        "Core type application of non-polymorphic type {function}"
                    ));
                };
                Ok(body.substitute(variable, argument))
            }
            // ANCHOR_END: type-rules
            Expr::Let(group, body) => {
                self.group(group, false)?;
                let result = self.expression(body);
                for b in &group.binds {
                    self.terms.remove(&b.name.unique());
                }
                result
            }
            Expr::If(condition, yes, no) => {
                let condition = self.expression(condition)?;
                self.expect(&condition, &Type::Bool)?;
                let yes = self.expression(yes)?;
                let no = self.expression(no)?;
                self.expect(&no, &yes)?;
                Ok(yes)
            }
            Expr::Prim(op, types, args, _) => {
                if !types.is_empty() || args.len() != 2 {
                    return Err("unsaturated Core primitive".into());
                }
                let operand = if *op == Op::Append {
                    Type::String
                } else {
                    Type::Int
                };
                for arg in args {
                    let ty = self.expression(arg)?;
                    self.expect(&ty, &operand)?;
                }
                Ok(if matches!(op, Op::Eq | Op::Lt | Op::Le) {
                    Type::Bool
                } else {
                    operand
                })
            }
        }
    }
    fn group(
        &mut self,
        group: &BindGroup,
        global: bool,
    ) -> Result<(), String> {
        for b in &group.binds {
            self.binder(&b.name)?;
            if global {
                self.globals.insert(b.name.unique());
            }
        }
        if group.recursive {
            for b in &group.binds {
                let mut body = &*b.rhs;
                while let Expr::TyLam(_, inner) = body {
                    body = inner;
                }
                if !matches!(body, Expr::Lam(..)) {
                    return Err("recursive Core non-function".into());
                }
                self.terms.insert(b.name.unique(), b.name.ty.clone());
            }
        }
        for b in &group.binds {
            let actual = self.expression(&b.rhs)?;
            self.expect(&actual, &b.name.ty)?;
        }
        if !group.recursive {
            for b in &group.binds {
                self.terms.insert(b.name.unique(), b.name.ty.clone());
            }
        }
        Ok(())
    }
}

/// Check kinds, scope, applications, and every binding's declared Core type.
// ANCHOR: core-lint
pub fn lint(program: &Program) -> Result<(), String> {
    let mut checker = Checker::default();
    for group in &program.groups {
        checker.group(group, true)?;
    }
    checker.expression(&Expr::Global(program.main.clone()))?;
    Ok(())
}
// ANCHOR_END: core-lint
