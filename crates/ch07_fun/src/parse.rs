//! The parser assigns spans to every surface node; no type or runtime decision is made here.
use crate::{
    ast::{Binding, Expr, Kind, Program},
    diagnostics::{Diagnostic, ErrorCode},
    lexer::{self, Token},
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
    let tokens = lexer::lex(sess, file)?;
    match parser::ProgramParser::new()
        .parse(file, tokens.into_iter().map(Ok))
    {
        Ok(program) => Ok(program),
        Err(error) => {
            sess.emit(diagnostic(file, error));
            Err(Stop)
        }
    }
}
fn diagnostic(
    file: FileId,
    error: ParseError<usize, Token, Diagnostic>,
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
                expected.join(", ")
            ),
        ),
        ParseError::UnrecognizedToken {
            token: (l, t, r),
            expected,
        } => (
            l,
            r,
            format!("unexpected {t:?}; expected {}", expected.join(", ")),
        ),
        ParseError::ExtraToken { token: (l, _, r) } => {
            (l, r, "unexpected trailing token".into())
        }
    };
    Diagnostic::error(ErrorCode::Parse, message, Span::new(file, l, r))
}
