//! Offside insertion with explicit delimiters and the restricted let/in rule.
use crate::{
    diagnostics::{Diagnostic, ErrorCode},
    lexer::{self, Spanned, Token},
    session::{Session, Stop},
    source::{FileId, Span},
};
use offsides::{Layout, LayoutConfig, LayoutLexer, LayoutMode};

#[derive(Clone, Debug)]
enum Item {
    Token(Token, bool),
    Group(Vec<Spanned>),
    Open,
    Close,
    Semi,
}
impl Layout for Item {
    fn v_open() -> Self {
        Self::Open
    }
    fn v_close() -> Self {
        Self::Close
    }
    fn v_sep() -> Self {
        Self::Semi
    }
}
fn opener(t: &Token) -> bool {
    matches!(t, Token::Let | Token::Where | Token::Of)
        || matches!(t, Token::Reserved(s) if matches!(s.as_str(), "where" | "of" | "do"))
}

/// Insert braces and semicolons. Groups shield outer indentation while their
/// contents still admit independent layout blocks, including parenthesized lets.
// ANCHOR: layout
pub fn layout(
    sess: &mut Session,
    file: FileId,
) -> Result<Vec<Spanned>, Stop> {
    let tokens = lexer::lex(sess, file)?;
    let source = &sess.sources.get(file).text;
    let eager = !matches!(tokens.first(), Some((_, Token::Open, _)));
    // offsides measures byte columns with fixed-width tabs. Give it a shadow
    // source whose byte positions are visual columns, then restore original spans.
    let (shadow, forward, backward) = visual_source(source);
    let tokens = tokens
        .into_iter()
        .map(|(lo, t, hi)| (forward[lo], t, forward[hi]))
        .collect::<Vec<_>>();
    match region(&shadow, &tokens, eager)
        .map(|tokens| {
            tokens
                .into_iter()
                .map(|(lo, t, hi)| (backward[lo], t, backward[hi]))
                .collect()
        })
        .map_err(|(lo, hi, message)| (backward[lo], backward[hi], message))
    {
        Ok(tokens) => Ok(tokens),
        Err((lo, hi, message)) => {
            sess.emit(Diagnostic::error(
                ErrorCode::Parse,
                message,
                Span::new(file, lo, hi),
            ));
            Err(Stop)
        }
    }
}
// ANCHOR_END: layout
type Failure = (usize, usize, &'static str);
fn region(
    source: &str,
    tokens: &[Spanned],
    eager: bool,
) -> Result<Vec<Spanned>, Failure> {
    let mut grouped = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (lo, token, hi) = &tokens[i];
        if matches!(token, Token::Open | Token::Left | Token::BracketOpen) {
            let close = if *token == Token::Open {
                Token::Close
            } else if *token == Token::BracketOpen {
                Token::BracketClose
            } else {
                Token::Right
            };
            let mut stack = vec![close];
            let mut end = i + 1;
            while end < tokens.len() && !stack.is_empty() {
                match &tokens[end].1 {
                    Token::Open => stack.push(Token::Close),
                    Token::Left => stack.push(Token::Right),
                    Token::BracketOpen => stack.push(Token::BracketClose),
                    t @ (Token::Close
                    | Token::Right
                    | Token::BracketClose)
                        if stack.pop().as_ref() != Some(t) =>
                    {
                        return Err((
                            tokens[end].0,
                            tokens[end].2,
                            "mismatched delimiter",
                        ));
                    }
                    _ => {}
                }
                end += 1;
            }
            if !stack.is_empty() {
                return Err((*lo, *hi, "unclosed delimiter"));
            }
            let mut inner = vec![tokens[i].clone()];
            inner.extend(region(source, &tokens[i + 1..end - 1], false)?);
            inner.push(tokens[end - 1].clone());
            grouped.push((*lo, Item::Group(inner), tokens[end - 1].2));
            i = end;
        } else {
            if matches!(
                token,
                Token::Close | Token::Right | Token::BracketClose
            ) {
                return Err((*lo, *hi, "unexpected closing delimiter"));
            }
            let implicit = opener(token)
                && i + 1 < tokens.len()
                && !matches!(tokens.get(i + 1), Some((_, Token::Open, _)));
            grouped.push((*lo, Item::Token(token.clone(), implicit), *hi));
            if opener(token) && i + 1 == tokens.len() {
                grouped.push((
                    *hi,
                    Item::Group(vec![
                        (*hi, Token::Open, *hi),
                        (*hi, Token::Close, *hi),
                    ]),
                    *hi,
                ));
            }
            i += 1;
        }
    }
    let config = LayoutConfig::new(|t| matches!(t, Item::Token(_, true)))
        .with_mode(if eager {
            LayoutMode::Eager
        } else {
            LayoutMode::Lazy
        })
        .with_tab_width(8);
    let stream = LayoutLexer::new(
        grouped.into_iter().map(Ok::<_, ()>),
        source,
        config,
    );
    let mut out = Vec::new();
    // offsides owns indentation frames. A frame closed early by `in` remains
    // in its stream until dedent; suppress that frame's subsequent punctuation.
    let mut frames: Vec<(bool, usize)> = Vec::new();
    let mut lets: Vec<Option<usize>> = Vec::new();
    let mut pending_let = false;
    for item in stream {
        let (lo, item, hi) = item.unwrap();
        match item {
            Item::Open => {
                if pending_let {
                    lets.push(Some(frames.len()));
                    pending_let = false;
                }
                let line = source[..lo]
                    .rsplit_once('\n')
                    .map_or(&source[..lo], |(_, s)| s);
                let column = line.chars().fold(0, |col, c| {
                    if c == '\t' {
                        (col / 8 + 1) * 8
                    } else {
                        col + 1
                    }
                });
                let empty = frames
                    .iter()
                    .rev()
                    .find(|f| f.0)
                    .is_some_and(|f| column <= f.1);
                out.push((lo, Token::Open, hi));
                if empty {
                    out.push((lo, Token::Close, hi));
                    for frame in frames.iter_mut().rev() {
                        if frame.0 && frame.1 > column {
                            out.push((lo, Token::Close, lo));
                            frame.0 = false;
                        }
                    }
                    if frames
                        .iter()
                        .rev()
                        .find(|f| f.0)
                        .is_some_and(|f| f.1 == column)
                    {
                        out.push((lo, Token::Semi, lo));
                    }
                }
                frames.push((!empty, column));
            }
            Item::Close => {
                let id = frames.len() - 1;
                for entry in &mut lets {
                    if *entry == Some(id) {
                        *entry = None;
                    }
                }
                if frames.pop().is_some_and(|f| f.0) {
                    out.push((lo, Token::Close, hi));
                }
            }
            Item::Semi => {
                let live = frames.iter().rev().find(|f| f.0);
                if frames
                    .last()
                    .zip(live)
                    .is_some_and(|(last, live)| last.1 == live.1)
                    && !matches!(
                        out.last(),
                        Some((_, Token::Semi | Token::Open, _))
                    )
                {
                    out.push((lo, Token::Semi, hi));
                }
            }
            Item::Group(inner) => out.extend(inner),
            Item::Token(token, implicit) => {
                if token == Token::In {
                    if matches!(out.last(), Some((a, Token::Semi, b)) if a == b)
                    {
                        out.pop();
                    }
                    if let Some(Some(id)) = lets.pop() {
                        for live in frames[id..].iter_mut().rev() {
                            if live.0 {
                                out.push((lo, Token::Close, lo));
                                live.0 = false;
                            }
                        }
                    }
                    // A newline before `in` may have produced a separator.
                    if matches!(out.last(), Some((a, Token::Semi, b)) if a == b)
                    {
                        out.pop();
                    }
                }
                if token == Token::Let {
                    pending_let = implicit;
                    if !implicit {
                        lets.push(None);
                    }
                }
                out.push((lo, token, hi));
            }
        }
    }
    Ok(out)
}

/// Preserve source coordinates while adapting Unicode columns and 8-column tab stops.
fn visual_source(source: &str) -> (String, Vec<usize>, Vec<usize>) {
    let mut shadow = String::new();
    let mut forward = vec![0; source.len() + 1];
    let mut backward = Vec::new();
    let mut column = 0;
    for (offset, c) in source.char_indices() {
        forward[offset] = shadow.len();
        let width = if c == '\t' { 8 - column % 8 } else { 1 };
        for _ in 0..width {
            backward.push(offset);
            shadow.push(if c == '\n' { '\n' } else { ' ' });
        }
        column = if c == '\n' { 0 } else { column + width };
        forward[offset + c.len_utf8()] = shadow.len();
    }
    backward.push(source.len());
    (shadow, forward, backward)
}
