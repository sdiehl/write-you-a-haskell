//! Maranget's usefulness recursion over constructor-specialized pattern matrices.
use crate::{
    core::{ConDecl, Lit, Match, Pat, RowBody, Type},
    diagnostics::{Diagnostic, ErrorCode, Severity},
    session::Session,
};
fn peel(p: &Pat) -> &Pat {
    if let Pat::As(_, p) = p { peel(p) } else { p }
}
fn wild(p: &Pat) -> bool {
    matches!(peel(p), Pat::Wild | Pat::Var(_))
}
fn same(a: &Pat, b: &Pat) -> bool {
    match (peel(a), peel(b)) {
        (Pat::Con(a, _), Pat::Con(b, _)) => a == b,
        (Pat::Lit(Lit::Double(a)), Pat::Lit(Lit::Double(b))) => {
            f64::from_bits(*a) == f64::from_bits(*b)
        }
        (Pat::Lit(a), Pat::Lit(b)) => a == b,
        _ => false,
    }
}
fn specialize(
    matrix: &[Vec<Pat>],
    head: &Pat,
    arity: usize,
) -> Vec<Vec<Pat>> {
    matrix
        .iter()
        .filter_map(|row| {
            let mut fields = if wild(&row[0]) {
                vec![Pat::Wild; arity]
            } else if same(&row[0], head) {
                match peel(&row[0]) {
                    Pat::Con(_, ps) => ps.clone(),
                    _ => vec![],
                }
            } else {
                return None;
            };
            fields.extend_from_slice(&row[1..]);
            Some(fields)
        })
        .collect()
}
fn fields(head: &Pat, ty: &Type, cons: &[ConDecl]) -> Vec<Type> {
    match peel(head) {
        Pat::Con(id, _) => {
            let c = &cons[id.index as usize];
            let args = match ty {
                Type::Con(_, args) => args.as_slice(),
                _ => &[],
            };
            c.fields
                .iter()
                .map(|t| {
                    crate::elaborate::instantiate(t, &c.parameters, args)
                })
                .collect()
        }
        _ => vec![],
    }
}
fn family(ty: &Type, cons: &[ConDecl]) -> Vec<Pat> {
    cons.iter()
        .filter(|c| match (&c.result, ty) {
            (Type::Bool, Type::Bool) => true,
            (Type::Con(a, _), Type::Con(b, _)) => a == b,
            _ => false,
        })
        .map(|c| Pat::Con(c.id.clone(), vec![Pat::Wild; c.fields.len()]))
        .collect()
}
/// Return a witness matched by the query but not by any covering row.
pub fn useful(
    matrix: &[Vec<Pat>],
    query: &[Pat],
    types: &[Type],
    cons: &[ConDecl],
) -> Option<Vec<Pat>> {
    if query.is_empty() {
        return matrix.is_empty().then(Vec::new);
    }
    let q = peel(&query[0]);
    if !wild(q) {
        let mut ts = fields(q, &types[0], cons);
        let n = ts.len();
        ts.extend_from_slice(&types[1..]);
        let mut qs = match q {
            Pat::Con(_, ps) => ps.clone(),
            _ => vec![],
        };
        qs.extend_from_slice(&query[1..]);
        let mut witness =
            useful(&specialize(matrix, q, n), &qs, &ts, cons)?;
        let children = witness.drain(..n).collect();
        let p = match q {
            Pat::Con(c, _) => Pat::Con(c.clone(), children),
            _ => q.clone(),
        };
        witness.insert(0, p);
        return Some(witness);
    }
    let heads = matrix
        .iter()
        .filter(|r| !wild(&r[0]))
        .map(|r| peel(&r[0]).clone())
        .collect::<Vec<_>>();
    let all = family(&types[0], cons);
    if !all.is_empty()
        && all.iter().all(|h| heads.iter().any(|p| same(h, p)))
    {
        for head in all {
            let mut ts = fields(&head, &types[0], cons);
            let n = ts.len();
            ts.extend_from_slice(&types[1..]);
            let mut qs = vec![Pat::Wild; n];
            qs.extend_from_slice(&query[1..]);
            if let Some(mut w) =
                useful(&specialize(matrix, &head, n), &qs, &ts, cons)
            {
                let children = w.drain(..n).collect();
                let Pat::Con(c, _) = head else { unreachable!() };
                w.insert(0, Pat::Con(c, children));
                return Some(w);
            }
        }
        None
    } else {
        let defaults = matrix
            .iter()
            .filter(|r| wild(&r[0]))
            .map(|r| r[1..].to_vec())
            .collect::<Vec<_>>();
        let mut witness =
            useful(&defaults, &query[1..], &types[1..], cons)?;
        let missing = all
            .into_iter()
            .find(|h| !heads.iter().any(|p| same(h, p)))
            .unwrap_or_else(|| {
                for n in 0..=heads.len() {
                    let p = match types[0] {
                        Type::Int => Pat::Lit(Lit::Int(n as i64)),
                        Type::Char => Pat::Lit(Lit::Char(
                            char::from_u32(97 + n as u32).unwrap_or('\0'),
                        )),
                        Type::String => {
                            Pat::Lit(Lit::String("x".repeat(n)))
                        }
                        Type::Double => {
                            Pat::Lit(Lit::Double((n as f64).to_bits()))
                        }
                        _ => return Pat::Wild,
                    };
                    if !heads.iter().any(|h| same(&p, h)) {
                        return p;
                    }
                }
                unreachable!()
            });
        witness.insert(0, missing);
        Some(witness)
    }
}
fn covers(
    body: &RowBody,
    truths: &std::collections::BTreeSet<u32>,
) -> bool {
    match body {
        RowBody::Plain(_) => true,
        RowBody::Guarded(gs) => gs.iter().any(|(g, _)| match g {
            crate::core::Expr::Con(c, _, _) => c.name == "True",
            crate::core::Expr::Global(id) => truths.contains(&id.unique()),
            _ => false,
        }),
    }
}
/// Warnings do not alter the decision tree; unmatched execution still fails at runtime.
pub fn warn(
    sess: &mut Session,
    m: &Match,
    cons: &[ConDecl],
    truths: &std::collections::BTreeSet<u32>,
) {
    let types = m.scruts.iter().map(|i| i.ty.clone()).collect::<Vec<_>>();
    let mut matrix = vec![];
    for (index, row) in m.rows.iter().enumerate() {
        if useful(&matrix, &row.pats, &types, cons).is_none()
            && !(index > 0 && m.rows[index - 1].span == row.span)
        {
            let mut d = Diagnostic::error(
                ErrorCode::Redundant,
                "clause can never match",
                row.span,
            );
            d.severity = Severity::Warning;
            sess.emit(d);
        }
        if covers(&row.body, truths) {
            matrix.push(row.pats.clone())
        }
    }
    let mut missing = vec![];
    for _ in 0..3 {
        let Some(p) =
            useful(&matrix, &vec![Pat::Wild; types.len()], &types, cons)
        else {
            break;
        };
        missing.push(
            p.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(" "),
        );
        matrix.push(p)
    }
    if !missing.is_empty() {
        let mut d = Diagnostic::error(
            ErrorCode::NonExhaustive,
            "non-exhaustive pattern match",
            m.span,
        );
        d.severity = Severity::Warning;
        d.notes.push(format!("missing: {}", missing.join("; ")));
        sess.emit(d)
    }
}
