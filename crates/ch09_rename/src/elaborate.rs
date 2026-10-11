//! Checked surface constructs become a separate, small executable language.
use crate::{
    ast::Op,
    core::{self, Bind, Expr, Lit},
    names::Id,
    renamed::{self, Kind},
    session::{Session, Stop},
    source::Span,
    typecheck::TypedProgram,
};
fn expression(expr: &renamed::Expr) -> Expr {
    let boxed = |e: &renamed::Expr| Box::new(expression(e));
    match &expr.kind {
        Kind::Int(n) => Expr::Lit(Lit::Int((n << 1) >> 1)),
        Kind::Bool(b) => Expr::Lit(Lit::Bool(*b)),
        Kind::Char(c) => Expr::Lit(Lit::Char(*c)),
        Kind::String(s) => Expr::Lit(Lit::String(s.clone())),
        Kind::Var(n) => Expr::Var(n.clone()),
        Kind::Lam(n, b) => Expr::Lam(n.clone(), boxed(b)),
        Kind::App(a, b) => Expr::App(boxed(a), boxed(b)),
        Kind::Let(groups, body) => {
            let mut body = expression(body);
            for g in groups.iter().rev() {
                body = Expr::Let(group(g), Box::new(body));
            }
            body
        }
        Kind::If(c, a, b) => Expr::If(boxed(c), boxed(a), boxed(b)),
        Kind::Binary(op, a, b) => {
            Expr::Prim(*op, vec![expression(a), expression(b)], expr.span)
        }
        Kind::Neg(a) => Expr::Prim(
            Op::Sub,
            vec![Expr::Lit(Lit::Int(0)), expression(a)],
            expr.span,
        ),
    }
}
fn group(group: &renamed::BindGroup) -> core::BindGroup {
    core::BindGroup {
        recursive: group.recursive,
        binds: group
            .binds
            .iter()
            .map(|b| Bind {
                name: b.id.clone(),
                rhs: Box::new(expression(&b.rhs)),
            })
            .collect(),
    }
}
fn primitive(
    sess: &mut Session,
    id: Id,
    op: Op,
    span: Span,
) -> core::BindGroup {
    let a = sess.fresh_id("$a");
    let b = sess.fresh_id("$b");
    let body = Expr::Prim(
        op,
        vec![Expr::Var(a.clone()), Expr::Var(b.clone())],
        span,
    );
    core::BindGroup {
        recursive: false,
        binds: vec![Bind {
            name: id,
            rhs: Box::new(Expr::Lam(
                a,
                Box::new(Expr::Lam(b, Box::new(body))),
            )),
        }],
    }
}
/// Lower dependency groups without changing their order or recursive scope.
pub fn elaborate(
    sess: &mut Session,
    typed: TypedProgram,
) -> Result<core::Program, Stop> {
    let mut groups = Vec::new();
    for (op, id) in typed.surface.primitives {
        groups.push(primitive(sess, id, op, typed.surface.span));
    }
    groups.extend(typed.surface.groups.iter().map(group));
    sess.finish(core::Program {
        groups,
        main: typed.surface.main,
    })
}
