//! Comment-preserving formatting through marginalia's document algebra.
use crate::{
    layout,
    lexer::{self, Spanned, Token},
    parse,
    session::{Session, Stop},
    source::FileId,
};
use marginalia::{
    attach::{AttachOptions, attach},
    pretty::{self, Doc, RenderOpts},
    span,
};

/// Validate syntax and render a canonical explicit-block form. Keeping all
/// original parentheses makes formatting independent of operator precedence.
// ANCHOR: format
pub fn format(sess: &mut Session, file: FileId) -> Result<String, Stop> {
    let parsed = parse::parse(sess, file)?;
    crate::fixity::resolve(sess, parsed)?;
    let (semantic, trivia) = lexer::trivia(sess, file)?;
    let tokens = layout::layout(sess, file)?;
    let source = &sess.sources.get(file).text;
    let mut comments = attach(
        source,
        &trivia,
        semantic.iter().map(|(lo, _, hi)| span(*lo, *hi)),
        AttachOptions::default(),
    );
    // Tokens are smaller anchors than statements. Put their comments on
    // separate leading lines so a trailing line comment cannot swallow a
    // newly inserted separator or a following `where`.
    for (lo, _, hi) in &semantic {
        let entry = comments.entry(span(*lo, *hi));
        entry.leading.append(&mut entry.trailing);
    }
    // Keep comments outside the lexically inseparable receiver-dot-label chain.
    for i in (1..semantic.len().saturating_sub(1)).rev() {
        if matches!(semantic[i].1, Token::Proj | Token::ProjSection) {
            let mut moved = Vec::new();
            for item in [&semantic[i], &semantic[i + 1]] {
                moved.append(
                    &mut comments.entry(span(item.0, item.2)).leading,
                );
            }
            let before = &semantic[i - 1];
            comments
                .entry(span(before.0, before.2))
                .leading
                .extend(moved);
        }
    }
    let mut cursor = 0;
    let doc = document(source, &tokens, &mut cursor, false);
    let rendered = pretty::render(&doc, &comments, RenderOpts::default());
    Ok(format!("{}\n", rendered.trim_end()))
}
// ANCHOR_END: format
fn token_doc(source: &str, item: &Spanned) -> Doc {
    let (lo, token, hi) = item;
    if lo != hi {
        return pretty::with_trivia(
            span(*lo, *hi),
            pretty::text(&source[*lo..*hi]),
        );
    }
    pretty::text(match token {
        Token::Open => "{",
        Token::Close => "}",
        Token::Semi => ";",
        _ => unreachable!("virtual punctuation"),
    })
}
fn document(
    source: &str,
    tokens: &[Spanned],
    cursor: &mut usize,
    nested: bool,
) -> Doc {
    let mut pieces = Vec::new();
    let mut need_space = false;
    let mut after_projection = false;
    while *cursor < tokens.len() {
        let item = &tokens[*cursor];
        if nested && item.1 == Token::Close {
            break;
        }
        *cursor += 1;
        match item.1 {
            Token::Open => {
                if need_space {
                    pieces.push(pretty::text(" "));
                }
                pieces.push(token_doc(source, item));
                let start = *cursor;
                let body = document(source, tokens, cursor, true);
                if *cursor == start {
                    pieces.push(pretty::text(" "));
                } else {
                    pieces.push(pretty::indent(
                        2,
                        pretty::concat([pretty::hardline(), body]),
                    ));
                    pieces.push(pretty::hardline());
                }
                pieces.push(token_doc(source, &tokens[*cursor]));
                *cursor += 1;
                need_space = true;
            }
            Token::Semi => {
                pieces.push(token_doc(source, item));
                // A closing brace already supplies the final newline.
                if !matches!(
                    tokens.get(*cursor),
                    Some((_, Token::Close, _))
                ) {
                    pieces.push(pretty::hardline());
                }
                need_space = false;
            }
            _ => {
                if need_space
                    && !after_projection
                    && !matches!(item.1, Token::Proj | Token::ProjSection)
                {
                    pieces.push(pretty::text(" "));
                }
                pieces.push(token_doc(source, item));
                after_projection =
                    matches!(item.1, Token::Proj | Token::ProjSection);
                need_space = true;
            }
        }
    }
    pretty::concat(pieces)
}
