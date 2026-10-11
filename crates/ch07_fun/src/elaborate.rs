//! Checked surface constructs become a separate, small executable language.
use crate::{
    ast::{self, Kind},
    core::{self, Bind, Expr, Lit},
    session::{Session, Stop},
    source::Span,
    typecheck::TypedProgram,
};
fn expression(expr: &ast::Expr) -> Expr {
    let boxed = |e: &ast::Expr| Box::new(expression(e));
    match &expr.kind {
        Kind::Int(n) => Expr::Lit(Lit::Int((n << 1) >> 1)),
        Kind::Bool(b) => Expr::Lit(Lit::Bool(*b)),
        Kind::Char(c) => Expr::Lit(Lit::Char(*c)),
        Kind::String(s) => Expr::Lit(Lit::String(s.clone())),
        Kind::Var(n) => Expr::Var(n.clone()),
        Kind::Lam(n, b) => Expr::Lam(n.clone(), boxed(b)),
        Kind::App(a, b) => Expr::App(boxed(a), boxed(b)),
        Kind::Let(n, a, b) => Expr::Let(binding(n, a), boxed(b)),
        Kind::If(c, a, b) => Expr::If(boxed(c), boxed(a), boxed(b)),
        Kind::Binary(op, a, b) => {
            Expr::Prim(*op, vec![expression(a), expression(b)], expr.span)
        }
        Kind::Neg(a) => Expr::Prim(
            ast::Op::Sub,
            vec![Expr::Lit(Lit::Int(0)), expression(a)],
            expr.span,
        ),
        Kind::Error => {
            unreachable!("the parser never returns recovery nodes")
        }
    }
}
fn binding(name: &str, rhs: &ast::Expr) -> Bind {
    Bind {
        name: name.into(),
        rhs: Box::new(expression(rhs)),
        recursive: ast::free(rhs).contains(name),
    }
}
fn primitive(name: &str, op: ast::Op, span: Span) -> Bind {
    Bind {
        name: name.into(),
        recursive: false,
        rhs: Box::new(Expr::Lam(
            "$a".into(),
            Box::new(Expr::Lam(
                "$b".into(),
                Box::new(Expr::Prim(
                    op,
                    vec![Expr::Var("$a".into()), Expr::Var("$b".into())],
                    span,
                )),
            )),
        )),
    }
}
/// Lower only after checking; recursive and lexical-scope invariants already hold.
pub fn elaborate(
    sess: &mut Session,
    typed: TypedProgram,
) -> Result<core::Program, Stop> {
    let mut bindings = vec![];
    if !sess.options.no_prelude {
        bindings.push(primitive("div", ast::Op::Div, typed.surface.span));
        bindings.push(primitive("mod", ast::Op::Mod, typed.surface.span));
    }
    bindings.extend(
        typed
            .surface
            .bindings
            .iter()
            .map(|b| binding(&b.name, &b.rhs)),
    );
    sess.finish(core::Program {
        bindings,
        main: "main".into(),
    })
}
