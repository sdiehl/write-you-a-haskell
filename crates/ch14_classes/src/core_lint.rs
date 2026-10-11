// adapted from sdiehl/typechecker-zoo/system-f/src/typecheck.rs (checking judgments)
//! Syntax-directed System F checking, with no call to the surface inferencer.
use crate::core::{
    BindGroup, ConDecl, Expr, Id, Lit, Pat, Program, RowBody, Type,
};
use std::collections::{BTreeMap, BTreeSet};
#[derive(Default)]
struct Checker {
    constructors: Vec<ConDecl>,
    arities: BTreeMap<crate::data::TyConId, crate::kinds::Kind>,
    type_kinds: BTreeMap<u32, crate::kinds::Kind>,
    terms: BTreeMap<u32, Type>,
    types: BTreeSet<u32>,
    seen_terms: BTreeSet<u32>,
    seen_types: BTreeSet<u32>,
    globals: BTreeSet<u32>,
}
impl Checker {
    fn well_formed(&self, ty: &Type) -> Result<(), String> {
        self.kind(ty)?;
        if let Some(v) = ty.free().difference(&self.types).next() {
            Err(format!("unbound Core type variable t{v}"))
        } else {
            Ok(())
        }
    }
    fn kind(&self, ty: &Type) -> Result<crate::kinds::Kind, String> {
        use crate::kinds::Kind as K;
        let apply = |f: K, a: K| match f {
            K::Arrow(x, b) if *x == a => Ok(*b),
            _ => Err("Core type application kind mismatch".into()),
        };
        match ty {
            Type::Var(v) => {
                Ok(self.type_kinds.get(v).cloned().unwrap_or(K::Star))
            }
            Type::Con(id, args) => {
                let mut k = if id.name == "(->)" {
                    K::arrow(K::Star, K::arrow(K::Star, K::Star))
                } else {
                    self.arities
                        .get(id)
                        .cloned()
                        .ok_or("unknown Core type constructor")?
                };
                for arg in args {
                    k = apply(k, self.kind(arg)?)?;
                }
                Ok(k)
            }
            Type::App(f, a) => apply(self.kind(f)?, self.kind(a)?),
            Type::Arrow(a, b) => {
                if self.kind(a)? != K::Star || self.kind(b)? != K::Star {
                    return Err(
                        "Core function operands must have kind *".into()
                    );
                }
                Ok(K::Star)
            }
            Type::Forall(_, b) => {
                if self.kind(b)? != K::Star {
                    return Err("Core quantification needs kind *".into());
                }
                Ok(K::Star)
            }
            _ => Ok(K::Star),
        }
    }
    fn constructor(
        &self,
        id: &crate::data::ConId,
    ) -> Result<ConDecl, String> {
        self.constructors
            .iter()
            .find(|c| &c.id == id)
            .cloned()
            .ok_or_else(|| "unknown Core constructor".into())
    }
    fn instantiate(&self, ty: &Type, vars: &[u32], args: &[Type]) -> Type {
        vars.iter()
            .zip(args)
            .fold(ty.clone(), |t, (v, a)| t.substitute(*v, a))
    }
    fn pattern(&mut self, pat: &Pat, ty: &Type) -> Result<(), String> {
        match pat {
            Pat::Wild => {}
            Pat::Var(id) | Pat::As(id, _) => {
                self.expect(&id.ty, ty)?;
                self.binder(id)?;
                self.terms.insert(id.unique(), ty.clone());
                if let Pat::As(_, p) = pat {
                    self.pattern(p, ty)?;
                }
            }
            Pat::Lit(l) => {
                let actual = self.expression(&Expr::Lit(l.clone()))?;
                self.expect(&actual, ty)?;
            }
            Pat::Con(id, ps) => {
                let c = self.constructor(id)?;
                let args = match ty {
                    Type::Con(_, args) => args.clone(),
                    Type::Bool => vec![],
                    _ => {
                        return Err(
                            "constructor pattern on non-data type".into()
                        );
                    }
                };
                if args.len() != c.parameters.len()
                    || ps.len() != c.fields.len()
                {
                    return Err("unsaturated Core pattern".into());
                }
                self.expect(
                    &self.instantiate(&c.result, &c.parameters, &args),
                    ty,
                )?;
                for (p, t) in ps.iter().zip(&c.fields) {
                    self.pattern(
                        p,
                        &self.instantiate(t, &c.parameters, &args),
                    )?;
                }
            }
        }
        Ok(())
    }
    fn binder(&mut self, id: &Id) -> Result<(), String> {
        self.well_formed(&id.ty)?;
        if self.kind(&id.ty)? != crate::kinds::Kind::Star {
            return Err("term binder type must have kind *".into());
        }
        if !self.seen_terms.insert(id.unique()) {
            return Err(format!("duplicate Core binder {}", id.name));
        }
        Ok(())
    }
    fn expect(&self, actual: &Type, expected: &Type) -> Result<(), String> {
        fn binder_kinds(checker: &Checker, a: &Type, b: &Type) -> bool {
            match (a, b) {
                (Type::Forall(x, a), Type::Forall(y, b)) => {
                    checker
                        .type_kinds
                        .get(x)
                        .cloned()
                        .unwrap_or(crate::kinds::Kind::Star)
                        == checker
                            .type_kinds
                            .get(y)
                            .cloned()
                            .unwrap_or(crate::kinds::Kind::Star)
                        && binder_kinds(checker, a, b)
                }
                (Type::Arrow(a, b), Type::Arrow(c, d))
                | (Type::App(a, b), Type::App(c, d)) => {
                    binder_kinds(checker, a, c)
                        && binder_kinds(checker, b, d)
                }
                (Type::Con(_, xs), Type::Con(_, ys)) => xs
                    .iter()
                    .zip(ys)
                    .all(|(x, y)| binder_kinds(checker, x, y)),
                _ => true,
            }
        }
        if actual.equivalent(expected)
            && binder_kinds(self, actual, expected)
        {
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
                Lit::Double(_) => Type::Double,
                Lit::Int(_) => Type::Int,
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
                if self.kind(argument)?
                    != self
                        .type_kinds
                        .get(&variable)
                        .cloned()
                        .unwrap_or(crate::kinds::Kind::Star)
                {
                    return Err("Core type argument has wrong kind".into());
                }
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
            Expr::Con(id, types, args) => {
                let c = self.constructor(id)?;
                if types.len() != c.parameters.len()
                    || args.len() != c.fields.len()
                {
                    return Err("unsaturated Core constructor".into());
                }
                for (v, t) in c.parameters.iter().zip(types) {
                    self.well_formed(t)?;
                    if self.kind(t)?
                        != self
                            .type_kinds
                            .get(v)
                            .cloned()
                            .unwrap_or(crate::kinds::Kind::Star)
                    {
                        return Err(
                            "Core constructor type argument has wrong kind"
                                .into(),
                        );
                    }
                }
                for (e, t) in args.iter().zip(&c.fields) {
                    let actual = self.expression(e)?;
                    self.expect(
                        &actual,
                        &self.instantiate(t, &c.parameters, types),
                    )?;
                }
                Ok(self.instantiate(&c.result, &c.parameters, types))
            }
            Expr::PatternFail(_, ty) => {
                self.well_formed(ty)?;
                Ok(ty.clone())
            }
            Expr::Match(m) => {
                self.well_formed(&m.result)?;
                for id in &m.scruts {
                    self.expression(&Expr::Var(id.clone()))?;
                }
                for row in &m.rows {
                    if row.pats.len() != m.scruts.len() {
                        return Err("Core match width mismatch".into());
                    }
                    let saved = self.terms.clone();
                    for (p, id) in row.pats.iter().zip(&m.scruts) {
                        self.pattern(p, &id.ty)?;
                    }
                    for group in &row.locals {
                        self.group(group, false)?;
                    }
                    match &row.body {
                        RowBody::Plain(e) => {
                            let t = self.expression(e)?;
                            self.expect(&t, &m.result)?;
                        }
                        RowBody::Guarded(gs) => {
                            for (g, e) in gs {
                                let t = self.expression(g)?;
                                self.expect(&t, &Type::Bool)?;
                                let t = self.expression(e)?;
                                self.expect(&t, &m.result)?;
                            }
                        }
                    }
                    self.terms = saved;
                }
                Ok(m.result.clone())
            }
            Expr::Prim(op, types, args, _) => {
                for t in types {
                    self.well_formed(t)?;
                    if self.kind(t)? != crate::kinds::Kind::Star {
                        return Err("primitive type argument kind".into());
                    }
                }
                let heads = self
                    .arities
                    .iter()
                    .map(|(id, k)| (id.clone(), k.clone()))
                    .collect::<Vec<_>>();
                let (mut ty, arity) =
                    crate::primitives::core_signature(*op, types, &heads)?;
                if args.len() != arity {
                    return Err("unsaturated Core primitive".into());
                }
                for arg in args {
                    let Type::Arrow(a, b) = ty else {
                        return Err("oversaturated Core primitive".into());
                    };
                    let actual = self.expression(arg)?;
                    self.expect(&actual, &a)?;
                    ty = *b;
                }
                Ok(ty)
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
    let mut checker = Checker {
        constructors: program.constructors.clone(),
        type_kinds: program.type_kinds.clone(),
        arities: program.data_types.iter().cloned().collect(),
        ..Checker::default()
    };
    for c in &program.constructors {
        for v in &c.parameters {
            if !checker.seen_types.insert(*v) {
                return Err("duplicate constructor type binder".into());
            }
            checker.types.insert(*v);
        }
        for t in c.fields.iter().chain(std::iter::once(&c.result)) {
            checker.well_formed(t)?;
            if checker.kind(t)? != crate::kinds::Kind::Star {
                return Err(
                    "Core constructor field must have kind *".into()
                );
            }
        }
        checker.types.clear();
    }
    for group in &program.groups {
        for b in &group.binds {
            checker.globals.insert(b.name.unique());
            checker.terms.insert(b.name.unique(), b.name.ty.clone());
        }
    }
    for group in &program.groups {
        checker.group(group, true)?;
    }
    checker.expression(&Expr::Global(program.main.clone()))?;
    Ok(())
}
// ANCHOR_END: core-lint
