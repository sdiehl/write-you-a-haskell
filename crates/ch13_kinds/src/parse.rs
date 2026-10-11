//! The parser assigns spans to every surface node; no type or runtime decision is made here.
use crate::{
    ast::{Binding, Expr, Kind, Program},
    diagnostics::{Diagnostic, ErrorCode},
    lexer::Token,
    session::{Session, Stop},
    source::{FileId, Span},
};
use lalrpop_util::{ParseError, lalrpop_mod};
lalrpop_mod!(
    #[allow(clippy::all)]
    parser
);
/// Equations retain patterns and where scopes until the desugaring pass.
pub fn equation(
    file: FileId,
    l: usize,
    r: usize,
    name: String,
    patterns: Vec<crate::ast::Pat>,
    body: crate::ast::RowBody,
    locals: Vec<Binding>,
) -> Binding {
    let span = Span::new(file, l, r);
    Binding {
        nonrecursive: vec![],
        name,
        rhs: Expr {
            kind: Kind::Equations(vec![crate::ast::Row {
                patterns,
                body,
                locals,
                span,
            }]),
            span,
        },
        span,
    }
}
/// Parse an explicit-brace program from a session's source file.
pub fn parse(sess: &mut Session, file: FileId) -> Result<Program, Stop> {
    let tokens = crate::layout::layout(sess, file)?;
    if tokens.is_empty() {
        sess.emit(Diagnostic::error(
            ErrorCode::Parse,
            "expected a declaration",
            Span::new(file, 0, 0),
        ));
        return Err(Stop);
    }
    match parser::ProgramParser::new()
        .parse(file, tokens.into_iter().map(Ok))
    {
        Ok(program) => Ok(program),
        Err(error) => {
            sess.emit(diagnostic(
                file,
                error,
                &sess.sources.get(file).text,
            ));
            Err(Stop)
        }
    }
}
fn diagnostic(
    file: FileId,
    error: ParseError<usize, Token, Diagnostic>,
    source: &str,
) -> Diagnostic {
    let (l, r, message) = match error {
        ParseError::User { error } => return error,
        ParseError::InvalidToken { location } => {
            (location, location, "invalid token".into())
        }
        ParseError::UnrecognizedEof { location, expected } => (
            location,
            location,
            format!(
                "unexpected end of input; expected {}",
                expected
                    .iter()
                    .take(6)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ),
        ParseError::UnrecognizedToken {
            token: (l, _, r),
            expected,
        } => (
            l,
            r,
            format!(
                "unexpected {}; expected {}",
                if l == r {
                    "layout boundary".into()
                } else {
                    format!("`{}`", &source[l..r])
                },
                expected
                    .iter()
                    .take(6)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ),
        ParseError::ExtraToken { token: (l, _, r) } => {
            (l, r, "unexpected trailing token".into())
        }
    };
    Diagnostic::error(ErrorCode::Parse, message, Span::new(file, l, r))
}

/// Retain a local declaration block as one scope for subsequent SCC analysis.
pub fn local(bindings: Vec<Binding>, body: Expr) -> Expr {
    if bindings.is_empty() {
        return body;
    }
    let span = bindings[0].span.through(body.span);
    Expr {
        kind: Kind::LetGroup(bindings, Box::new(body)),
        span,
    }
}

/// Attach signatures irrespective of their position within the declaration block.
pub fn declarations(
    decls: Vec<crate::ast::Declaration>,
) -> Result<Vec<Binding>, Diagnostic> {
    use crate::ast::Declaration;
    let mut bindings: Vec<Binding> = Vec::new();
    let mut signatures = std::collections::BTreeMap::new();
    for decl in decls {
        match decl {
            Declaration::Alias(a) => {
                return Err(Diagnostic::error(
                    ErrorCode::Parse,
                    "type synonyms are top-level only",
                    a.span,
                ));
            }
            Declaration::Data(d) => {
                return Err(Diagnostic::error(
                    ErrorCode::Parse,
                    "data declarations are top-level only",
                    d.span,
                ));
            }
            Declaration::Fixity(f) => {
                return Err(Diagnostic::error(
                    ErrorCode::Parse,
                    "fixity declarations are top-level only",
                    f.span,
                ));
            }
            Declaration::Pattern(pat, body, locals, span) => {
                let name = format!("$pattern{}", bindings.len());
                bindings.extend(pattern_binding(
                    pat, body, locals, span, name,
                )?);
            }
            Declaration::Binding(binding) => {
                if let Some(previous) = bindings.last_mut()
                    && previous.name == binding.name
                    && let (Kind::Equations(rows), Kind::Equations(next)) =
                        (&mut previous.rhs.kind, &binding.rhs.kind)
                    && !rows[0].patterns.is_empty()
                {
                    if rows[0].patterns.len() != next[0].patterns.len() {
                        return Err(Diagnostic::error(
                            ErrorCode::Mismatch,
                            "equations must have the same number of arguments",
                            binding.span,
                        ));
                    }
                    rows.extend(next.clone());
                    previous.span = previous.span.through(binding.span);
                    continue;
                }
                bindings.push(binding);
            }
            Declaration::Signature(names, ty, span) => {
                for name in names {
                    if signatures
                        .insert(name.clone(), (ty.clone(), span))
                        .is_some()
                    {
                        return Err(Diagnostic::error(
                            ErrorCode::Duplicate,
                            format!("duplicate signature for {name}"),
                            span,
                        ));
                    }
                }
            }
        }
    }
    for binding in &mut bindings {
        if let Some((scheme, _)) = signatures.remove(&binding.name) {
            binding.rhs = Expr {
                span: binding.rhs.span,
                kind: Kind::Annotated(
                    Box::new(binding.rhs.clone()),
                    scheme,
                ),
            };
        }
    }
    if let Some((name, (_, span))) = signatures.into_iter().next() {
        return Err(Diagnostic::error(
            ErrorCode::Unbound,
            format!("signature without a binding: {name}"),
            span,
        ));
    }
    Ok(bindings)
}
/// Separate global fixities from ordinary declarations before resolving chains.
pub fn program(
    decls: Vec<crate::ast::Declaration>,
    span: Span,
) -> Result<Program, Diagnostic> {
    let mut fixities = Vec::new();
    let mut data = Vec::new();
    let mut aliases = Vec::new();
    let mut bindings = Vec::new();
    for decl in decls {
        match decl {
            crate::ast::Declaration::Fixity(f) => fixities.push(f),
            crate::ast::Declaration::Data(d) => data.push(d),
            crate::ast::Declaration::Alias(a) => aliases.push(a),
            other => bindings.push(other),
        }
    }
    Ok(Program {
        bindings: declarations(bindings)?,
        fixities,
        data,
        aliases,
        span,
    })
}
pub fn fixity(
    associativity: crate::ast::Associativity,
    precedence: i64,
    operators: Vec<String>,
    span: Span,
) -> Result<crate::ast::Fixity, Diagnostic> {
    if !(0..=9).contains(&precedence) {
        return Err(Diagnostic::error(
            ErrorCode::Parse,
            "fixity precedence must be a digit from 0 to 9",
            span,
        ));
    }
    Ok(crate::ast::Fixity {
        associativity,
        precedence: precedence as u8,
        operators,
        span,
    })
}

/// Separate a function equation from a strict, non-recursive pattern binding.
pub fn lhs(
    mut head: crate::ast::Pat,
    rest: Vec<crate::ast::Pat>,
    body: crate::ast::RowBody,
    locals: Vec<Binding>,
    span: Span,
) -> Result<crate::ast::Declaration, Diagnostic> {
    use crate::ast::{Declaration, PatKind};
    if let PatKind::Var(name) = head.kind {
        return Ok(Declaration::Binding(equation(
            span.file,
            span.start as usize,
            span.end as usize,
            name,
            rest,
            body,
            locals,
        )));
    }
    if !rest.is_empty() {
        if let PatKind::Con(_, ps) = &mut head.kind {
            ps.extend(rest);
        } else {
            return Err(Diagnostic::error(
                ErrorCode::Parse,
                "only a constructor pattern can have arguments",
                span,
            ));
        }
    }
    Ok(Declaration::Pattern(head, body, locals, span))
}
fn pattern_binding(
    pat: crate::ast::Pat,
    body: crate::ast::RowBody,
    locals: Vec<Binding>,
    span: Span,
    name: String,
) -> Result<Vec<Binding>, Diagnostic> {
    use crate::ast::{Pat, PatKind, Row, RowBody};
    fn names(p: &Pat, out: &mut Vec<String>) -> Result<(), Diagnostic> {
        match &p.kind {
            PatKind::Record(_, fields) => {
                for (_, p) in fields {
                    names(p, out)?;
                }
            }
            PatKind::Var(n) | PatKind::As(n, _) => {
                if out.contains(n) {
                    return Err(Diagnostic::error(
                        ErrorCode::Duplicate,
                        "duplicate pattern variable",
                        p.span,
                    ));
                }
                out.push(n.clone());
                if let PatKind::As(_, p) = &p.kind {
                    names(p, out)?;
                }
            }
            PatKind::Con(_, ps)
            | PatKind::List(ps)
            | PatKind::Tuple(ps) => {
                for p in ps {
                    names(p, out)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
    let mut bound = vec![];
    names(&pat, &mut bound)?;
    let expr = |kind| Expr { kind, span };
    let bundle = bound
        .iter()
        .rev()
        .fold(expr(Kind::Tuple(vec![])), |tail, n| {
            expr(Kind::Tuple(vec![expr(Kind::Var(n.clone())), tail]))
        });
    let rhs = expr(Kind::Equations(vec![Row {
        patterns: vec![],
        body,
        locals,
        span,
    }]));
    let checked = expr(Kind::Case(
        Box::new(rhs),
        vec![Row {
            patterns: vec![pat],
            body: RowBody::Plain(Box::new(bundle)),
            locals: vec![],
            span,
        }],
    ));
    let mut result = vec![Binding {
        name: name.clone(),
        rhs: checked,
        span,
        nonrecursive: bound.clone(),
    }];
    for (index, n) in bound.iter().enumerate() {
        let pattern = bound.iter().enumerate().rev().fold(
            Pat {
                kind: PatKind::Tuple(vec![]),
                span,
            },
            |tail, (i, _)| Pat {
                kind: PatKind::Tuple(vec![
                    Pat {
                        kind: if i == index {
                            PatKind::Var("$selected".into())
                        } else {
                            PatKind::Wild
                        },
                        span,
                    },
                    tail,
                ]),
                span,
            },
        );
        let rhs = expr(Kind::Case(
            Box::new(expr(Kind::Var(name.clone()))),
            vec![Row {
                patterns: vec![pattern],
                body: RowBody::Plain(Box::new(expr(Kind::Var(
                    "$selected".into(),
                )))),
                locals: vec![],
                span,
            }],
        ));
        result.push(Binding {
            name: n.clone(),
            rhs,
            span,
            nonrecursive: vec![],
        });
    }
    Ok(result)
}
