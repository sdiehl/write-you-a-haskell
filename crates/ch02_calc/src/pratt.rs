//! A hand-written binding-power parser for comparison with the generated LR parser.
use crate::{
    ast::{Expr, Kind, Op},
    diagnostic::Diagnostic,
    lexer::{self, Spanned, Token},
};

/// Parse the same language and produce the same AST, including byte spans.
pub fn parse(source: &str) -> Result<Expr, Vec<Diagnostic>> {
    let mut parser = Parser {
        tokens: lexer::lex(source)?,
        at: 0,
        eof: source.len(),
    };
    let result = parser.expression().and_then(|value| {
        if parser.at == parser.tokens.len() {
            Ok(value)
        } else {
            Err(parser.error("unexpected trailing token"))
        }
    });
    result.map_err(|e| vec![e])
}

struct Parser {
    tokens: Vec<Spanned>,
    at: usize,
    eof: usize,
}

impl Parser {
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.at).map(|(_, t, _)| t)
    }
    fn error(&self, message: &str) -> Diagnostic {
        let span = self
            .tokens
            .get(self.at)
            .map_or(self.eof..self.eof, |(a, _, b)| *a..*b);
        Diagnostic::new("E0002", message, span)
    }
    fn expect(&mut self, wanted: Token) -> Result<(), Diagnostic> {
        if self.peek() == Some(&wanted) {
            self.at += 1;
            Ok(())
        } else {
            Err(self.error(&format!("expected {wanted:?}")))
        }
    }
    fn expression(&mut self) -> Result<Expr, Diagnostic> {
        if self.peek() != Some(&Token::Let) {
            return self.infix(0);
        }
        let start = self.tokens[self.at].0;
        self.at += 1;
        let Some(Token::Ident(name)) = self.peek().cloned() else {
            return Err(self.error("expected a binding name"));
        };
        self.at += 1;
        self.expect(Token::Equals)?;
        let value = self.expression()?;
        self.expect(Token::In)?;
        let body = self.expression()?;
        let end = body.span.end;
        Ok(Expr::new(
            Kind::Let(name, Box::new(value), Box::new(body)),
            start,
            end,
        ))
    }
    // ANCHOR: pratt
    fn infix(&mut self, min_power: u8) -> Result<Expr, Diagnostic> {
        let mut left = self.prefix()?;
        loop {
            let (op, power) = match self.peek() {
                Some(Token::Plus) => (Op::Add, 1),
                Some(Token::Minus) => (Op::Sub, 1),
                Some(Token::Star) => (Op::Mul, 3),
                Some(Token::Slash) => (Op::Div, 3),
                _ => break,
            };
            if power < min_power {
                break;
            }
            self.at += 1;
            let right = self.infix(power + 1)?;
            left = Expr::binary(op, left, right);
        }
        Ok(left)
    }
    // ANCHOR_END: pratt
    fn prefix(&mut self) -> Result<Expr, Diagnostic> {
        let Some((start, token, end)) = self.tokens.get(self.at).cloned()
        else {
            return Err(self.error("expected expression"));
        };
        self.at += 1;
        match token {
            Token::Int(n) => Ok(Expr::new(Kind::Int(n), start, end)),
            Token::Ident(name) => {
                Ok(Expr::new(Kind::Var(name), start, end))
            }
            Token::Minus => {
                let value = self.prefix()?;
                let end = value.span.end;
                Ok(Expr::new(Kind::Neg(Box::new(value)), start, end))
            }
            Token::Left => {
                let mut value = self.expression()?;
                self.expect(Token::Right)?;
                value.span = start..self.tokens[self.at - 1].2;
                Ok(value)
            }
            _ => {
                self.at -= 1;
                Err(self.error(
                    "expected integer, variable, negation, or parentheses",
                ))
            }
        }
    }
}
