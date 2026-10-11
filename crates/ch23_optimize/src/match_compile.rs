//! First-row decision trees with shared right-hand sides and guard-failure joins.
use crate::{
    core::{self, Alt, AltCon, Expr, Id, Pat, RowBody, Type},
    names,
    session::{Session, Stop},
    source::Span,
};
use std::collections::BTreeMap;
#[derive(Clone)]
struct Row {
    pats: Vec<Pat>,
    values: BTreeMap<u32, Id>,
    target: names::Id,
    params: Vec<Id>,
}
struct Compiler<'a> {
    sess: &'a mut Session,
    constructors: Vec<core::ConDecl>,
    truths: std::collections::BTreeSet<u32>,
}
impl Compiler<'_> {
    fn fresh(&mut self, ty: Type) -> Id {
        Id {
            name: self.sess.fresh_id("$case"),
            ty,
        }
    }
    fn fields(&self, con: &crate::data::ConId, ty: &Type) -> Vec<Type> {
        let c = &self.constructors[con.index as usize];
        let args = match ty {
            Type::Con(_, args) => args.as_slice(),
            Type::Bool => &[],
            _ => unreachable!("typed data scrutinee"),
        };
        c.fields
            .iter()
            .map(|t| crate::elaborate::instantiate(t, &c.parameters, args))
            .collect()
    }
    fn boolean(
        &mut self,
        test: Expr,
        yes: Expr,
        no: Expr,
        result: &Type,
    ) -> Expr {
        let truth = self
            .constructors
            .iter()
            .find(|c| c.id.name == "True")
            .unwrap()
            .id
            .clone();
        Expr::Case(
            Box::new(test),
            self.fresh(Type::Bool),
            result.clone(),
            vec![
                Alt {
                    con: AltCon::Con(truth),
                    binders: vec![],
                    rhs: yes,
                },
                Alt {
                    con: AltCon::Default,
                    binders: vec![],
                    rhs: no,
                },
            ],
        )
    }
    fn group(&mut self, g: core::BindGroup) -> core::BindGroup {
        core::BindGroup {
            recursive: g.recursive,
            binds: g
                .binds
                .into_iter()
                .map(|mut b| {
                    b.rhs = Box::new(self.expr(*b.rhs));
                    b
                })
                .collect(),
        }
    }
    fn expr(&mut self, e: Expr) -> Expr {
        match e {
            Expr::Match(m) => self.compile(m),
            Expr::Lam(i, b) => Expr::Lam(i, Box::new(self.expr(*b))),
            Expr::TyLam(v, b) => Expr::TyLam(v, Box::new(self.expr(*b))),
            Expr::TyApp(b, t) => Expr::TyApp(Box::new(self.expr(*b)), t),
            e @ Expr::App(..) => {
                let mut work = vec![Some(e)];
                let mut output = Vec::new();
                while let Some(job) = work.pop() {
                    match job {
                        Some(Expr::App(a, b)) => {
                            work.push(None);
                            work.push(Some(*b));
                            work.push(Some(*a));
                        }
                        Some(e) => output.push(self.expr(e)),
                        None => {
                            let b = output.pop().unwrap();
                            let a = output.pop().unwrap();
                            output
                                .push(Expr::App(Box::new(a), Box::new(b)));
                        }
                    }
                }
                output.pop().unwrap()
            }
            Expr::Let(g, b) => {
                Expr::Let(self.group(g), Box::new(self.expr(*b)))
            }
            Expr::Con(c, ts, args) => Expr::Con(
                c,
                ts,
                args.into_iter().map(|e| self.expr(e)).collect(),
            ),
            Expr::Prim(p, ts, args, s) => Expr::Prim(
                p,
                ts,
                args.into_iter().map(|e| self.expr(e)).collect(),
                s,
            ),
            other => other,
        }
    }
    fn compile(&mut self, m: core::Match) -> Expr {
        crate::usefulness::warn(
            self.sess,
            &m,
            &self.constructors,
            &self.truths,
        );
        let mut rows = vec![];
        let mut bodies = vec![];
        for r in m.rows {
            let mut params = vec![];
            for p in &r.pats {
                binders(p, &mut params)
            }
            let target = self.sess.fresh_id("$rhs");
            rows.push(Row {
                pats: r.pats,
                values: BTreeMap::new(),
                target: target.clone(),
                params: params.clone(),
            });
            bodies.push((target, params, r.locals, r.body));
        }
        let mut definitions = vec![];
        for (index, (target, params, locals, body)) in
            bodies.into_iter().enumerate().rev()
        {
            let fail = self.sess.fresh_id("$fail");
            let mut body = match body {
                RowBody::Plain(e) => self.expr(*e),
                RowBody::Guarded(gs) => {
                    let residual = self.tree(
                        m.scruts.clone(),
                        rows[index + 1..].to_vec(),
                        &m.result,
                        m.span,
                    );
                    definitions.push((fail.clone(), vec![], residual));
                    let mut fallback =
                        Expr::Jump(fail.clone(), vec![], m.result.clone());
                    for (g, e) in gs.into_iter().rev() {
                        let g = self.expr(g);
                        let e = self.expr(e);
                        fallback = self.boolean(g, e, fallback, &m.result)
                    }
                    fallback
                }
            };
            for g in locals.into_iter().rev() {
                body = Expr::Let(self.group(g), Box::new(body))
            }
            definitions.push((target, params, body));
        }
        let mut body = self.tree(m.scruts, rows, &m.result, m.span);
        for (id, params, rhs) in definitions.into_iter().rev() {
            body = Expr::Join(id, params, Box::new(rhs), Box::new(body))
        }
        body
    }
    fn tree(
        &mut self,
        scruts: Vec<Id>,
        mut rows: Vec<Row>,
        result: &Type,
        span: Span,
    ) -> Expr {
        if rows.is_empty() {
            return Expr::PatternFail(span, result.clone());
        }
        // Bind variables/as-patterns without copying their unique Core binders.
        for row in &mut rows {
            for (p, s) in row.pats.iter_mut().zip(&scruts) {
                loop {
                    match p.clone() {
                        Pat::Var(id) => {
                            row.values.insert(id.unique(), s.clone());
                            *p = Pat::Wild;
                            break;
                        }
                        Pat::As(id, inner) => {
                            row.values.insert(id.unique(), s.clone());
                            *p = *inner
                        }
                        _ => break,
                    }
                }
            }
        }
        let Some(column) =
            rows[0].pats.iter().position(|p| !matches!(p, Pat::Wild))
        else {
            let row = &rows[0];
            return Expr::Jump(
                row.target.clone(),
                row.params
                    .iter()
                    .map(|p| Expr::Var(row.values[&p.unique()].clone()))
                    .collect(),
                result.clone(),
            );
        };
        let scrut = scruts[column].clone();
        let mut heads = vec![];
        for row in &rows {
            let head = match &row.pats[column] {
                Pat::Con(c, _) => AltCon::Con(c.clone()),
                Pat::Lit(l) => AltCon::Lit(l.clone()),
                _ => continue,
            };
            if !heads.contains(&head) {
                heads.push(head)
            }
        }
        let mut alts = vec![];
        for head in heads {
            let fields = match &head {
                AltCon::Con(c) => self.fields(c, &scrut.ty),
                _ => vec![],
            };
            let params = fields
                .into_iter()
                .map(|t| self.fresh(t))
                .collect::<Vec<_>>();
            let mut specialized = vec![];
            for mut row in rows.clone() {
                let p = row.pats.remove(column);
                let fields = match (&head, p) {
                    (_, Pat::Wild) => vec![Pat::Wild; params.len()],
                    (AltCon::Con(c), Pat::Con(d, ps)) if *c == d => ps,
                    (AltCon::Lit(a), Pat::Lit(b)) if *a == b => vec![],
                    _ => continue,
                };
                row.pats.splice(column..column, fields);
                specialized.push(row)
            }
            let mut columns = scruts.clone();
            columns.splice(column..=column, params.clone());
            let rhs = self.tree(columns, specialized, result, span);
            alts.push(Alt {
                con: head,
                binders: params,
                rhs,
            });
        }
        let mut defaults = vec![];
        for mut row in rows {
            if matches!(row.pats[column], Pat::Wild) {
                row.pats.remove(column);
                defaults.push(row)
            }
        }
        let mut columns = scruts;
        columns.remove(column);
        let fallback = self.tree(columns, defaults, result, span);
        if matches!(scrut.ty, Type::String | Type::Double) {
            let mut body = fallback;
            for alt in alts.into_iter().rev() {
                let AltCon::Lit(l) = alt.con else {
                    unreachable!()
                };
                let primitive = if matches!(scrut.ty, Type::String) {
                    crate::primitives::Prim::StrEq
                } else {
                    crate::primitives::Prim::DblEq
                };
                let test = Expr::Prim(
                    crate::ast::Op::Runtime(primitive),
                    vec![],
                    vec![Expr::Var(scrut.clone()), Expr::Lit(l)],
                    span,
                );
                body = self.boolean(test, alt.rhs, body, result);
            }
            body
        } else {
            alts.push(Alt {
                con: AltCon::Default,
                binders: vec![],
                rhs: fallback,
            });
            Expr::Case(
                Box::new(Expr::Var(scrut.clone())),
                self.fresh(scrut.ty),
                result.clone(),
                alts,
            )
        }
    }
}
fn binders(p: &Pat, out: &mut Vec<Id>) {
    match p {
        Pat::Var(i) => out.push(i.clone()),
        Pat::As(i, p) => {
            out.push(i.clone());
            binders(p, out)
        }
        Pat::Con(_, ps) => {
            for p in ps {
                binders(p, out)
            }
        }
        _ => {}
    }
}
/// Compile every nested match; generated binders come from the shared unique supply.
pub fn compile(
    sess: &mut Session,
    mut program: core::Program,
) -> Result<core::Program, Stop> {
    let mut compiler = Compiler {
        sess,
        constructors: program.constructors.clone(),
        truths: program.groups.iter().flat_map(|g| &g.binds)
            .filter(|b| matches!(&*b.rhs, Expr::Con(c, _, _) if c.name == "True"))
            .map(|b| b.name.unique()).collect(),
    };
    program.groups = program
        .groups
        .into_iter()
        .map(|g| compiler.group(g))
        .collect();
    compiler.sess.finish(program)
}
