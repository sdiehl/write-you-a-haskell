//! Logos supplies maximal-munch tokens and byte spans.
use crate::diagnostic::Diagnostic;
use logos::Logos;

// ANCHOR: tokens
#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(skip r"[ \t\r\n]+")]
pub enum Token {
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Equals,
    #[token("(")]
    Left,
    #[token(")")]
    Right,
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),
    #[regex(r"[a-z_][A-Za-z0-9_']*", |lex| lex.slice().to_owned())]
    Ident(String),
}
// ANCHOR_END: tokens

pub type Spanned = (usize, Token, usize);

/// Return every lexical error rather than disguising invalid input as a token.
pub fn lex(source: &str) -> Result<Vec<Spanned>, Vec<Diagnostic>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for (token, span) in Token::lexer(source).spanned() {
        match token {
            Ok(token) => tokens.push((span.start, token, span.end)),
            Err(()) => errors.push(Diagnostic::new(
                "E0001",
                "invalid token or integer literal outside i64 range",
                span,
            )),
        }
    }
    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}
