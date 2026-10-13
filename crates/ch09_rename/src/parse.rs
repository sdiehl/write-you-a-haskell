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

/// Fold equation parameters rightward into curried lambdas, rejecting duplicate parameters.
pub fn lambda(
    file: FileId,
    l: usize,
    r: usize,
    params: Vec<String>,
    mut body: Expr,
) -> Result<Expr, Diagnostic> {
    let mut names = std::collections::HashSet::new();
    for name in &params {
        if !names.insert(name) {
            return Err(Diagnostic::error(
                ErrorCode::Duplicate,
                format!("duplicate parameter {name}"),
                Span::new(file, l, r),
            ));
        }
    }
    for name in params.into_iter().rev() {
        body = Expr::new(Kind::Lam(name, Box::new(body)), file, l, r);
    }
    Ok(body)
}

pub fn binding(
    file: FileId,
    l: usize,
    r: usize,
    name: String,
    params: Vec<String>,
    rhs: Expr,
) -> Result<Binding, Diagnostic> {
    Ok(Binding {
        name,
        rhs: lambda(file, l, r, params, rhs)?,
        span: Span::new(file, l, r),
    })
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
    let mut bindings = Vec::new();
    let mut signatures = std::collections::BTreeMap::new();
    for decl in decls {
        match decl {
            Declaration::Fixity(f) => {
                return Err(Diagnostic::error(
                    ErrorCode::Parse,
                    "fixity declarations are top-level only",
                    f.span,
                ));
            }
            Declaration::Binding(binding) => bindings.push(binding),
            Declaration::Signature(names, ty, span) => {
                for name in names {
                    if signatures
                        .insert(name.clone(), (signature(&ty, span)?, span))
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

fn signature(
    syntax: &crate::ast::TypeSyntax,
    span: Span,
) -> Result<crate::types::Scheme, Diagnostic> {
    use crate::{
        ast::TypeSyntax,
        types::{Scheme, Type},
    };
    fn convert(
        syntax: &TypeSyntax,
        names: &mut std::collections::BTreeMap<String, u32>,
        span: Span,
    ) -> Result<Type, Diagnostic> {
        Ok(match syntax {
            TypeSyntax::Name(n) => match n.as_str() {
                "Int" => Type::Int,
                "Bool" => Type::Bool,
                "Char" => Type::Char,
                "String" => Type::String,
                _ if n.starts_with(char::is_lowercase)
                    || n.starts_with('_') =>
                {
                    let next = names.len() as u32;
                    Type::Var(*names.entry(n.clone()).or_insert(next))
                }
                _ => {
                    return Err(Diagnostic::error(
                        ErrorCode::UnboundConstructor,
                        format!("unknown type {n}"),
                        span,
                    ));
                }
            },
            TypeSyntax::Arrow(a, b) => Type::arrow(
                convert(a, names, span)?,
                convert(b, names, span)?,
            ),
        })
    }
    let mut names = std::collections::BTreeMap::new();
    let ty = convert(syntax, &mut names, span)?;
    Ok(Scheme {
        variables: (0..names.len() as u32).collect(),
        ty,
    })
}

/// Separate global fixities from ordinary declarations before resolving chains.
pub fn program(
    decls: Vec<crate::ast::Declaration>,
    span: Span,
) -> Result<Program, Diagnostic> {
    let mut fixities = Vec::new();
    let mut bindings = Vec::new();
    for decl in decls {
        match decl {
            crate::ast::Declaration::Fixity(f) => fixities.push(f),
            other => bindings.push(other),
        }
    }
    Ok(Program {
        bindings: declarations(bindings)?,
        fixities,
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
