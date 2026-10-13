//! Logos tokens retain byte offsets; comments nest and literals decode Unicode escapes.
use crate::{
    diagnostics::{Diagnostic, ErrorCode},
    session::{Session, Stop},
    source::{FileId, Span},
};
use logos::{FilterResult, Lexer, Logos};

/// The final reserved-word table also generates both book syntax definitions.
pub const KEYWORDS: &[&str] = &[
    "case", "class", "data", "deriving", "do", "else", "if", "in", "infix",
    "infixl", "infixr", "instance", "let", "of", "then", "type", "where",
];

/// Operator characters use maximal munch, even for syntax introduced in later snapshots.
pub const SYMBOLS: &str = "!#$%&*+./<=>?@\\^|~:-";

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(skip r"[ \t\r\n]+")]
pub enum Token {
    #[token("deriving")]
    Deriving,
    #[token("..")]
    Range,
    #[token("{-", block_comment)]
    BlockComment(String),
    #[regex(r"--+", line_comment, priority = 4)]
    LineComment(String),
    #[token("class")]
    Class,
    #[token("instance")]
    Instance,
    #[token("=>")]
    Constraint,
    #[token("type")]
    Type,
    #[token("data")]
    Data,
    #[token("case")]
    Case,
    #[token("of")]
    Of,
    #[token(":")]
    Colon,
    #[token("|")]
    Bar,
    #[token("@")]
    At,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("infixl")]
    Infixl,
    #[token("infixr")]
    Infixr,
    #[token("infix")]
    Infix,
    #[token("`")]
    Backtick,
    #[token("where")]
    Where,
    #[token("True")]
    True,
    #[token("False")]
    False,
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("++")]
    Append,
    #[token("==")]
    Eq,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token("::")]
    ColonColon,
    #[token(",")]
    Comma,
    #[token("=")]
    Equals,
    #[token("(")]
    Left,
    #[token(")")]
    Right,
    #[token("{")]
    Open,
    #[token("}")]
    Close,
    #[token(";")]
    Semi,
    #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+", |lex| lex.slice().parse::<f64>().unwrap().to_bits())]
    Double(u64),
    #[regex(r"[0-9][0-9_]*|0x[0-9a-fA-F_]+", integer)]
    Int(i64),
    #[regex(r#""([^"\\\n]|\\[^\n])*""#, |lex|decode(lex.slice()))]
    String(String),
    #[regex(r"'([^'\\\n]|\\[^\n])*'", character)]
    Char(char),
    #[regex(r"[a-z_][A-Za-z0-9_']*", |lex|lex.slice().to_owned())]
    Ident(String),
    #[regex(r"[A-Z][A-Za-z0-9_']*", |lex|lex.slice().to_owned())]
    Constructor(String),
    #[regex(r"[!#$%&*+./<=>?@\\^|~:\-]+", |lex| lex.slice().to_owned(), priority = 1)]
    Operator(String),
    #[token("do")]
    Do,
    #[token("<-")]
    BindArrow,
    /// An adjacent record projection dot.
    Proj,
    ProjSection,
}

fn integer(lex: &mut Lexer<Token>) -> Option<i64> {
    let text = lex.slice().replace('_', "");
    let value = if let Some(hex) = text.strip_prefix("0x") {
        i64::from_str_radix(hex, 16).ok()
    } else {
        text.parse::<i64>().ok()
    }?;
    Some((value << 1) >> 1)
}

fn character(lex: &mut Lexer<Token>) -> Option<char> {
    let text = decode(lex.slice())?;
    let mut chars = text.chars();
    let first = chars.next()?;
    if chars.next().is_none() {
        Some(first)
    } else {
        None
    }
}

/// Decode exactly the escape forms specified for Fun, rejecting invalid scalar values.
pub fn decode(quoted: &str) -> Option<String> {
    let mut out = String::new();
    let mut chars = quoted[1..quoted.len() - 1].chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        out.push(match chars.next()? {
            'n' => '\n',
            't' => '\t',
            '0' => '\0',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            'u' => {
                if chars.next()? != '{' {
                    return None;
                }
                let mut digits = String::new();
                loop {
                    let c = chars.next()?;
                    if c == '}' {
                        break;
                    }
                    if !c.is_ascii_hexdigit() {
                        return None;
                    }
                    digits.push(c);
                }
                char::from_u32(u32::from_str_radix(&digits, 16).ok()?)?
            }
            _ => return None,
        });
    }
    Some(out)
}

fn block_comment(lex: &mut Lexer<Token>) -> FilterResult<String, ()> {
    let text = lex.remainder();
    let bytes = text.as_bytes();
    let (mut depth, mut i) = (1, 0);
    while i + 1 < bytes.len() {
        match &bytes[i..i + 2] {
            b"{-" => {
                depth += 1;
                i += 2;
            }
            b"-}" => {
                depth -= 1;
                i += 2;
                if depth == 0 {
                    lex.bump(i);
                    return FilterResult::Emit(lex.slice().to_owned());
                }
            }
            _ => i += 1,
        }
    }
    lex.bump(text.len());
    FilterResult::Error(())
}

fn line_comment(lex: &mut Lexer<Token>) -> FilterResult<String, ()> {
    if lex
        .remainder()
        .chars()
        .next()
        .is_some_and(|c| SYMBOLS.contains(c))
    {
        return FilterResult::Emit(lex.slice().to_owned());
    }
    let end = lex.remainder().find('\n').unwrap_or(lex.remainder().len());
    lex.bump(end);
    FilterResult::Emit(lex.slice().to_owned())
}

pub type Spanned = (usize, Token, usize);

/// Lex one session source and collect all invalid-token diagnostics before stopping.
pub fn raw(sess: &mut Session, file: FileId) -> Result<Vec<Spanned>, Stop> {
    let source = &sess.sources.get(file).text;
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for (token, range) in Token::lexer(source).spanned() {
        match token {
            Ok(token) => tokens.push((range.start, token, range.end)),
            Err(()) => errors.push(Diagnostic::error(
                ErrorCode::Lex,
                "invalid token, escape, or integer outside i64 range",
                Span::new(file, range.start, range.end),
            )),
        }
    }
    for (_, token, _) in &tokens {
        if let Token::Ident(n) | Token::Constructor(n) = token {
            sess.interner.intern(n);
        }
    }
    projection_dots(file, &mut tokens, &mut errors);
    sess.diagnostics.extend(errors);
    sess.finish(tokens)
}

impl marginalia::Classify for Token {
    fn trivia(&self) -> Option<marginalia::TriviaPiece<'_>> {
        use marginalia::{BuiltinKind, TriviaPiece};
        match self {
            Self::LineComment(text) => Some(TriviaPiece {
                kind: BuiltinKind::Line,
                text,
            }),
            Self::BlockComment(text) => Some(TriviaPiece {
                kind: BuiltinKind::Block,
                text,
            }),
            _ => None,
        }
    }
}

/// Capture trivia before handing semantic tokens to layout.
pub fn trivia(
    sess: &mut Session,
    file: FileId,
) -> Result<(Vec<Spanned>, marginalia::TriviaTable), Stop> {
    let tokens = raw(sess, file)?;
    let mut lexer = marginalia::TriviaLexer::new(
        tokens.into_iter().map(Ok::<_, ()>),
        &sess.sources.get(file).text,
    );
    let tokens = lexer.by_ref().map(Result::unwrap).collect();
    Ok((tokens, lexer.into_table()))
}

/// Lex semantic tokens without inserting layout punctuation.
pub fn lex(sess: &mut Session, file: FileId) -> Result<Vec<Spanned>, Stop> {
    Ok(trivia(sess, file)?.0)
}

// ANCHOR: projection-dots
/// Classify dot adjacency before trivia capture and layout can erase spacing.
fn projection_dots(
    file: FileId,
    tokens: &mut [Spanned],
    errors: &mut Vec<Diagnostic>,
) {
    for i in 1..tokens.len().saturating_sub(1) {
        if !matches!(&tokens[i].1,Token::Operator(op) if op==".")
            || tokens[i].2 != tokens[i + 1].0
            || !matches!(tokens[i + 1].1, Token::Ident(_))
        {
            continue;
        }
        if tokens[i - 1].2 != tokens[i].0 {
            continue;
        }
        if tokens[i - 1].1 == Token::Left {
            tokens[i].1 = Token::ProjSection;
            if !matches!(
                tokens[i + 2..].iter().find(|(_, t, _)| !matches!(
                    t,
                    Token::LineComment(_) | Token::BlockComment(_)
                )),
                Some((_, Token::Right, _))
            ) {
                errors.push(Diagnostic::error(
                    ErrorCode::ProjectionDot,
                    "a projection section must have the form (.field)",
                    Span::new(file, tokens[i].0, tokens[i].2),
                ));
            }
        } else if matches!(
            tokens[i - 1].1,
            Token::Ident(_)
                | Token::Right
                | Token::BracketClose
                | Token::Close
                | Token::Double(_)
                | Token::Int(_)
                | Token::Char(_)
                | Token::String(_)
        ) {
            tokens[i].1 = Token::Proj;
        }
    }
}
// ANCHOR_END: projection-dots
