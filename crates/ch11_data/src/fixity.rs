//! Resolve flat operator chains using the Haskell precedence and associativity rules.
use crate::{
    ast::{Associativity as Assoc, Expr, Kind, Op, Program},
    diagnostics::{Diagnostic, ErrorCode},
    session::{Session, Stop},
    source::Span,
};
use std::collections::{BTreeMap, BTreeSet};
#[derive(Clone, Copy)]
struct Fix {
    precedence: u8,
    assoc: Assoc,
}
fn default_fixity(op: &str) -> Fix {
    let (precedence, assoc) = match op {
        "." => (9, Assoc::Right),
        "*" | "/" | "div" | "mod" | "quot" | "rem" => (7, Assoc::Left),
        "+" | "-" => (6, Assoc::Left),
        "<>" => (6, Assoc::Right),
        ":" | "++" => (5, Assoc::Right),
        "==" | "/=" | "<" | "<=" | ">" | ">=" => (4, Assoc::None),
        "&&" => (3, Assoc::Right),
        "||" => (2, Assoc::Right),
        "<$>" | "<*>" => (4, Assoc::Left),
        ">>=" | ">>" => (1, Assoc::Left),
        "$" => (0, Assoc::Right),
        _ => (9, Assoc::Left),
    };
    Fix { precedence, assoc }
}
struct Resolver {
    table: BTreeMap<String, Fix>,
    shadowed: BTreeSet<String>,
    supply: usize,
}
impl Resolver {
    fn fix(&self, op: &str) -> Fix {
        self.table
            .get(op)
            .copied()
            .unwrap_or_else(|| default_fixity(op))
    }
    fn error(&self, span: Span, message: &str) -> Diagnostic {
        Diagnostic::error(ErrorCode::Parse, message, span)
    }
    fn binary(&self, op: &str, a: Expr, b: Expr) -> Expr {
        let span = a.span.through(b.span);
        if op == "&&" || op == "||" {
            let fixed = Expr {
                kind: Kind::Var(
                    if op == "&&" { "False" } else { "True" }.into(),
                ),
                span,
            };
            let (yes, no) =
                if op == "&&" { (b, fixed) } else { (fixed, b) };
            return Expr {
                kind: Kind::If(Box::new(a), Box::new(yes), Box::new(no)),
                span,
            };
        }
        let primitive = if self.shadowed.contains(op) {
            None
        } else {
            match op {
                "+" => Some(Op::Add),
                "-" => Some(Op::Sub),
                "*" => Some(Op::Mul),
                "++" => Some(Op::Append),
                "==" => Some(Op::Eq),
                "<" => Some(Op::Lt),
                "<=" => Some(Op::Le),
                _ => None,
            }
        };
        if let Some(op) = primitive {
            return Expr::binary(op, a, b);
        }
        let var = Expr {
            kind: Kind::Var(op.into()),
            span,
        };
        let app = Expr {
            kind: Kind::App(Box::new(var), Box::new(a)),
            span,
        };
        Expr {
            kind: Kind::App(Box::new(app), Box::new(b)),
            span,
        }
    }
    fn fresh(&mut self, span: Span) -> Expr {
        let name = format!("$section{}", self.supply);
        self.supply += 1;
        Expr {
            kind: Kind::Var(name),
            span,
        }
    }
    fn section(
        &mut self,
        op: String,
        value: Expr,
        left: bool,
        span: Span,
    ) -> Result<Expr, Diagnostic> {
        if let Kind::Chain(_, pairs, _) = &value.kind {
            for (inner, _) in pairs {
                let (a, b) = (self.fix(inner), self.fix(&op));
                let expected =
                    if left { Assoc::Left } else { Assoc::Right };
                if a.precedence < b.precedence
                    || a.precedence == b.precedence
                        && (a.assoc != expected || b.assoc != expected)
                {
                    return Err(self.error(span,"section operand requires parentheses at this fixity"));
                }
            }
        }
        let value = self.expr(value)?;
        let argument = self.fresh(span);
        let Kind::Var(name) = argument.kind.clone() else {
            unreachable!()
        };
        let body = if left {
            self.binary(&op, value, argument)
        } else {
            self.binary(&op, argument, value)
        };
        Ok(Expr {
            kind: Kind::Lam(name, Box::new(body)),
            span,
        })
    }
    fn reduce(&self, values: &mut Vec<Expr>, operator: Option<String>) {
        let right = values.pop().expect("chain operand");
        let result = if let Some(op) = operator {
            let left = values.pop().expect("left operand");
            self.binary(&op, left, right)
        } else {
            let span = right.span;
            Expr {
                kind: Kind::Neg(Box::new(right)),
                span,
            }
        };
        values.push(result);
    }
    fn operand(
        &mut self,
        value: Expr,
        values: &mut Vec<Expr>,
        operators: &mut Vec<Option<String>>,
    ) -> Result<(), Diagnostic> {
        if let Kind::Neg(inner) = value.kind {
            if operators.last().is_some_and(|op| {
                op.as_ref().is_none_or(|op| self.fix(op).precedence >= 6)
            }) {
                return Err(self.error(value.span,"parenthesize negation after an operator of precedence 6 or higher"));
            }
            operators.push(None);
            values.push(self.expr(*inner)?);
        } else {
            values.push(self.expr(value)?);
        }
        Ok(())
    }
    // ANCHOR: chain
    fn chain(
        &mut self,
        first: Expr,
        pairs: Vec<(String, Expr)>,
    ) -> Result<Expr, Diagnostic> {
        let mut values = Vec::new();
        let mut operators: Vec<Option<String>> = Vec::new();
        self.operand(first, &mut values, &mut operators)?;
        for (op, value) in pairs {
            let next = self.fix(&op);
            while let Some(previous) = operators.last() {
                let prior =
                    previous.as_ref().map(|p| self.fix(p)).unwrap_or(Fix {
                        precedence: 6,
                        assoc: Assoc::Left,
                    });
                if prior.precedence < next.precedence {
                    break;
                }
                if prior.precedence == next.precedence && previous.is_some()
                {
                    if prior.assoc != next.assoc
                        || prior.assoc == Assoc::None
                    {
                        return Err(self.error(value.span,"operators with conflicting fixities need parentheses"));
                    }
                    if prior.assoc == Assoc::Right {
                        break;
                    }
                }
                let prior = operators.pop().unwrap();
                self.reduce(&mut values, prior);
            }
            operators.push(Some(op));
            self.operand(value, &mut values, &mut operators)?;
        }
        while let Some(op) = operators.pop() {
            self.reduce(&mut values, op);
        }
        Ok(values.pop().unwrap())
    }
    // ANCHOR_END: chain
    fn row(
        &mut self,
        mut row: crate::ast::Row,
    ) -> Result<crate::ast::Row, Diagnostic> {
        row.body = match row.body {
            crate::ast::RowBody::Plain(e) => {
                crate::ast::RowBody::Plain(Box::new(self.expr(*e)?))
            }
            crate::ast::RowBody::Guarded(gs) => {
                crate::ast::RowBody::Guarded(
                    gs.into_iter()
                        .map(|(g, e)| Ok((self.expr(g)?, self.expr(e)?)))
                        .collect::<Result<_, Diagnostic>>()?,
                )
            }
        };
        for b in &mut row.locals {
            b.rhs = self.expr(b.rhs.clone())?;
        }
        Ok(row)
    }
    fn expr(&mut self, expr: Expr) -> Result<Expr, Diagnostic> {
        let span = expr.span;
        let kind = match expr.kind {
            Kind::Var(op) if op == "&&" || op == "||" => {
                let a = self.fresh(span);
                let b = self.fresh(span);
                let (Kind::Var(x), Kind::Var(y)) =
                    (a.kind.clone(), b.kind.clone())
                else {
                    unreachable!()
                };
                let body = self.binary(&op, a, b);
                Kind::Lam(
                    x,
                    Box::new(Expr {
                        kind: Kind::Lam(y, Box::new(body)),
                        span,
                    }),
                )
            }
            Kind::Equations(rows) => Kind::Equations(
                rows.into_iter()
                    .map(|r| self.row(r))
                    .collect::<Result<_, _>>()?,
            ),
            Kind::PatternLam(ps, e) => {
                Kind::PatternLam(ps, Box::new(self.expr(*e)?))
            }
            Kind::Case(e, rows) => Kind::Case(
                Box::new(self.expr(*e)?),
                rows.into_iter()
                    .map(|r| self.row(r))
                    .collect::<Result<_, _>>()?,
            ),
            Kind::List(es) => Kind::List(
                es.into_iter()
                    .map(|e| self.expr(e))
                    .collect::<Result<_, _>>()?,
            ),
            Kind::Tuple(es) => Kind::Tuple(
                es.into_iter()
                    .map(|e| self.expr(e))
                    .collect::<Result<_, _>>()?,
            ),
            Kind::Chain(a, pairs, None) => return self.chain(*a, pairs),
            Kind::Chain(_, _, Some(_)) => {
                return Err(
                    self.error(span, "a section must be parenthesized")
                );
            }
            Kind::Paren(a) => {
                if let Kind::Chain(first, pairs, Some(op)) = a.kind {
                    let operand = Expr {
                        kind: Kind::Chain(first, pairs, None),
                        span: a.span,
                    };
                    return self.section(op, operand, true, span);
                }
                return self.expr(*a);
            }
            Kind::RightSection(op, a) => {
                return self.section(op, *a, false, span);
            }
            Kind::Lam(n, a) => Kind::Lam(n, Box::new(self.expr(*a)?)),
            Kind::App(a, b) => Kind::App(
                Box::new(self.expr(*a)?),
                Box::new(self.expr(*b)?),
            ),
            Kind::LetGroup(mut bindings, body) => {
                for b in &mut bindings {
                    b.rhs = self.expr(b.rhs.clone())?;
                }
                Kind::LetGroup(bindings, Box::new(self.expr(*body)?))
            }
            Kind::If(c, a, b) => Kind::If(
                Box::new(self.expr(*c)?),
                Box::new(self.expr(*a)?),
                Box::new(self.expr(*b)?),
            ),
            Kind::Annotated(a, s) => {
                Kind::Annotated(Box::new(self.expr(*a)?), s)
            }
            Kind::Neg(a) => Kind::Neg(Box::new(self.expr(*a)?)),
            other => other,
        };
        Ok(Expr { kind, span })
    }
}
/// Resolve all chains only after collecting the file's fixity declarations.
pub fn resolve(
    sess: &mut Session,
    program: Program,
) -> Result<Program, Stop> {
    resolve_with_prelude(sess, program, &[])
}
/// Prelude declarations provide defaults, while declarations in this file take precedence.
pub fn resolve_with_prelude(
    sess: &mut Session,
    mut program: Program,
    inherited: &[crate::ast::Fixity],
) -> Result<Program, Stop> {
    fn bound(expr: &Expr, names: &mut BTreeSet<String>) {
        match &expr.kind {
            Kind::Equations(rows) | Kind::Case(_, rows) => {
                if let Kind::Case(e, _) = &expr.kind {
                    bound(e, names);
                }
                for row in rows {
                    for b in &row.locals {
                        names.insert(b.name.clone());
                        bound(&b.rhs, names);
                    }
                    match &row.body {
                        crate::ast::RowBody::Plain(e) => bound(e, names),
                        crate::ast::RowBody::Guarded(gs) => {
                            for (g, e) in gs {
                                bound(g, names);
                                bound(e, names);
                            }
                        }
                    }
                }
            }
            Kind::PatternLam(_, e) => bound(e, names),
            Kind::List(es) | Kind::Tuple(es) => {
                for e in es {
                    bound(e, names);
                }
            }
            Kind::LetGroup(bindings, body) => {
                for b in bindings {
                    names.insert(b.name.clone());
                    bound(&b.rhs, names);
                }
                bound(body, names);
            }
            Kind::Lam(_, a)
            | Kind::Neg(a)
            | Kind::Paren(a)
            | Kind::Annotated(a, _)
            | Kind::RightSection(_, a) => bound(a, names),
            Kind::App(a, b) => {
                bound(a, names);
                bound(b, names);
            }
            Kind::If(c, a, b) => {
                bound(c, names);
                bound(a, names);
                bound(b, names);
            }
            Kind::Chain(a, pairs, _) => {
                bound(a, names);
                for (_, b) in pairs {
                    bound(b, names);
                }
            }
            _ => {}
        }
    }
    let mut resolver = Resolver {
        table: BTreeMap::new(),
        shadowed: BTreeSet::new(),
        supply: 0,
    };
    for binding in &program.bindings {
        resolver.shadowed.insert(binding.name.clone());
        bound(&binding.rhs, &mut resolver.shadowed);
    }
    for name in &resolver.shadowed {
        if matches!(name.as_str(), "&&" | "||") {
            sess.emit(Diagnostic::error(
                ErrorCode::ReservedOperator,
                "short-circuit operators cannot be redefined",
                program.span,
            ));
        }
        if name.starts_with(':') {
            sess.emit(Diagnostic::error(
                ErrorCode::Parse,
                "user infix constructors are not supported",
                program.span,
            ));
        }
    }
    for fixity in inherited {
        for op in &fixity.operators {
            resolver.table.insert(
                op.clone(),
                Fix {
                    precedence: fixity.precedence,
                    assoc: fixity.associativity,
                },
            );
        }
    }
    let mut declared = BTreeSet::new();
    for fixity in &program.fixities {
        for op in &fixity.operators {
            resolver.table.insert(
                op.clone(),
                Fix {
                    precedence: fixity.precedence,
                    assoc: fixity.associativity,
                },
            );
            if !declared.insert(op.clone()) {
                sess.emit(Diagnostic::error(
                    ErrorCode::Duplicate,
                    format!("duplicate fixity for {op}"),
                    fixity.span,
                ));
            }
        }
    }
    sess.finish(())?;
    for binding in &mut program.bindings {
        match resolver.expr(binding.rhs.clone()) {
            Ok(expr) => binding.rhs = expr,
            Err(error) => sess.emit(error),
        }
    }
    sess.finish(program)
}
