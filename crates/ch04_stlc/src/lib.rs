//! Chapter 4: checking simple types before evaluation.
pub mod ast;
pub mod diagnostic;
pub mod eval;
pub mod lexer;
pub mod names;
pub mod normal;
pub mod types;

use ast::Expr;
use diagnostic::Diagnostic;
use lalrpop_util::{ParseError, lalrpop_mod};
use lexer::Token;

lalrpop_mod!(
    #[allow(clippy::all)]
    parser
);

// ANCHOR: parse
/// Parse and recover enough to report several syntax errors, then stop the pass.
pub fn parse(source: &str) -> Result<Expr, Vec<Diagnostic>> {
    let tokens = lexer::lex(source)?;
    let mut recovered = Vec::new();
    let parsed = parser::ExprParser::new()
        .parse(&mut recovered, tokens.into_iter().map(Ok));
    let mut errors: Vec<_> = recovered
        .into_iter()
        .map(|r| parse_error(r.error))
        .collect();
    match parsed {
        Ok(expr) if errors.is_empty() => Ok(expr),
        Ok(_) => Err(errors),
        Err(error) => {
            errors.push(parse_error(error));
            Err(errors)
        }
    }
}
// ANCHOR_END: parse

fn parse_error(error: ParseError<usize, Token, Diagnostic>) -> Diagnostic {
    let (span, message) = match error {
        ParseError::InvalidToken { location } => {
            (location..location, "invalid token".into())
        }
        ParseError::UnrecognizedEof { location, expected } => (
            location..location,
            format!(
                "unexpected end of input; expected {}",
                expected.join(", ")
            ),
        ),
        ParseError::UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => (
            start..end,
            format!(
                "unexpected {token:?}; expected {}",
                expected.join(", ")
            ),
        ),
        ParseError::ExtraToken {
            token: (start, _, end),
        } => (start..end, "unexpected trailing token".into()),
        ParseError::User { error } => return error,
    };
    Diagnostic::new("E0002", message, span)
}
