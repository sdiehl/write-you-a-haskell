//! IO is opaque in source types and the unit-to-result function constructor in Core.
use crate::core::{self, Expr, Type};
/// Rewrite every occurrence, including higher-kinded IO arguments and dictionary fields.
pub fn lower(program: &mut core::Program) {
    let Some((unit, _)) =
        program.data_types.iter().find(|(id, _)| id.name == "()")
    else {
        return;
    };
    let unit = Type::Con(unit.clone(), vec![]);
    let arrow = program
        .data_types
        .iter()
        .find(|(id, _)| id.name == "(->)")
        .expect("built-in arrow is registered")
        .0
        .clone();
    fn ty(t: &mut Type, unit: &Type, arrow: &crate::data::TyConId) {
        match t {
            Type::Con(id, args) => {
                for a in args.iter_mut() {
                    ty(a, unit, arrow)
                }
                if id.name == "IO" {
                    *t = args.iter().cloned().fold(
                        Type::Con(arrow.clone(), vec![unit.clone()]),
                        Type::apply,
                    );
                }
            }
            Type::Arrow(a, b) => {
                ty(a, unit, arrow);
                ty(b, unit, arrow)
            }
            Type::App(a, b) => {
                ty(a, unit, arrow);
                ty(b, unit, arrow);
                *t = Type::apply(*a.clone(), *b.clone())
            }
            Type::Forall(_, b) => ty(b, unit, arrow),
            _ => {}
        }
    }
    fn id(i: &mut core::Id, u: &Type, a: &crate::data::TyConId) {
        ty(&mut i.ty, u, a)
    }
    fn pat(p: &mut core::Pat, u: &Type, a: &crate::data::TyConId) {
        match p {
            core::Pat::Var(i) => id(i, u, a),
            core::Pat::As(i, p) => {
                id(i, u, a);
                pat(p, u, a)
            }
            core::Pat::Con(_, ps) => {
                for p in ps {
                    pat(p, u, a)
                }
            }
            _ => {}
        }
    }
    fn group(g: &mut core::BindGroup, u: &Type, a: &crate::data::TyConId) {
        for b in &mut g.binds {
            id(&mut b.name, u, a);
            expr(&mut b.rhs, u, a)
        }
    }
    fn expr(e: &mut Expr, u: &Type, a: &crate::data::TyConId) {
        match e {
            Expr::Case(s, i, t, alts) => {
                expr(s, u, a);
                id(i, u, a);
                ty(t, u, a);
                for alt in alts {
                    for i in &mut alt.binders {
                        id(i, u, a)
                    }
                    expr(&mut alt.rhs, u, a)
                }
            }
            Expr::Join(_, ps, b, r) => {
                for i in ps {
                    id(i, u, a)
                }
                expr(b, u, a);
                expr(r, u, a)
            }
            Expr::Jump(_, es, t) => {
                for e in es {
                    expr(e, u, a)
                }
                ty(t, u, a)
            }
            Expr::Var(i) | Expr::Global(i) => id(i, u, a),
            Expr::TyLam(_, b) => expr(b, u, a),
            Expr::TyApp(b, t) => {
                expr(b, u, a);
                ty(t, u, a)
            }
            Expr::Lam(i, b) => {
                id(i, u, a);
                expr(b, u, a)
            }
            Expr::App(f, x) => {
                expr(f, u, a);
                expr(x, u, a)
            }
            Expr::Let(g, b) => {
                group(g, u, a);
                expr(b, u, a)
            }
            Expr::Con(_, ts, es) | Expr::Prim(_, ts, es, _) => {
                for t in ts {
                    ty(t, u, a)
                }
                for e in es {
                    expr(e, u, a)
                }
            }
            Expr::PatternFail(_, t) => ty(t, u, a),
            Expr::Lit(_) => {}
            Expr::Match(m) => {
                for i in &mut m.scruts {
                    id(i, u, a)
                }
                ty(&mut m.result, u, a);
                for r in &mut m.rows {
                    for p in &mut r.pats {
                        pat(p, u, a)
                    }
                    for g in &mut r.locals {
                        group(g, u, a)
                    }
                    match &mut r.body {
                        core::RowBody::Plain(b) => expr(b, u, a),
                        core::RowBody::Guarded(gs) => {
                            for (g, b) in gs {
                                expr(g, u, a);
                                expr(b, u, a)
                            }
                        }
                    }
                }
            }
        }
    }
    for g in &mut program.groups {
        group(g, &unit, &arrow)
    }
    id(&mut program.main, &unit, &arrow);
    for c in &mut program.constructors {
        for t in &mut c.fields {
            ty(t, &unit, &arrow)
        }
        ty(&mut c.result, &unit, &arrow)
    }
}
