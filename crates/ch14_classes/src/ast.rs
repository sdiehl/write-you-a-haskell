//! Named lambda terms and a precedence-aware printer.
use crate::source::{FileId, Span};
use std::fmt;

// ANCHOR: ast
#[derive(Clone, Debug, PartialEq, Eq)]
/// A named, unannotated term with a source span.
pub struct Expr {
    pub kind: Kind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// The forms of the extended lambda calculus.
pub enum Kind {
    Double(u64),
    Int(i64),
    Bool(bool),
    Char(char),
    String(String),
    Var(String),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetGroup(Vec<Binding>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Chain(Box<Expr>, Vec<(String, Expr)>, Option<String>),
    Paren(Box<Expr>),
    RightSection(String, Box<Expr>),
    Annotated(Box<Expr>, TypeSyntax),
    Record(String, Vec<(String, Expr)>),
    Project(Box<Expr>, String),
    Update(Box<Expr>, Vec<(String, Expr)>),
    ProjectionSection(String),
    Equations(Vec<Row>),
    PatternLam(Vec<Pat>, Box<Expr>),
    Case(Box<Expr>, Vec<Row>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Error,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Monomorphic integer arithmetic and comparisons.
pub enum Op {
    Runtime(crate::primitives::Prim),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Append,
    Eq,
    Lt,
    Le,
}
// ANCHOR_END: ast

impl Op {
    pub fn text(self) -> &'static str {
        match self {
            Self::Runtime(p) => p.name(),
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Append => "++",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
        }
    }
    pub fn precedence(self) -> u8 {
        match self {
            Self::Eq | Self::Lt | Self::Le => 1,
            Self::Add | Self::Sub => 3,
            Self::Mul | Self::Div | Self::Mod => 5,
            Self::Append => 2,
            Self::Runtime(_) => 9,
        }
    }
}

impl Expr {
    pub fn new(kind: Kind, file: FileId, start: usize, end: usize) -> Self {
        Self {
            kind,
            span: Span::new(file, start, end),
        }
    }
    pub fn binary(op: Op, a: Self, b: Self) -> Self {
        let (file, start, end) =
            (a.span.file, a.span.start as usize, b.span.end as usize);
        Self::new(
            Kind::Binary(op, Box::new(a), Box::new(b)),
            file,
            start,
            end,
        )
    }
    fn print(
        &self,
        f: &mut fmt::Formatter<'_>,
        context: u8,
    ) -> fmt::Result {
        let power = match &self.kind {
            Kind::Lam(..)
            | Kind::Let(..)
            | Kind::LetGroup(..)
            | Kind::If(..) => 0,
            Kind::Binary(op, ..) => op.precedence(),
            Kind::Neg(..) => 6,
            Kind::Int(n) if *n < 0 => 6,
            Kind::App(..) => 7,
            _ => 9,
        };
        let parens = power < context;
        if parens {
            write!(f, "(")?;
        }
        match &self.kind {
            Kind::Double(bits) => write!(
                f,
                "{}",
                crate::primitives::show_double(f64::from_bits(*bits))
            )?,
            Kind::Int(n) => write!(f, "{n}")?,
            Kind::Bool(b) => {
                write!(f, "{}", if *b { "True" } else { "False" })?
            }
            Kind::Char(c) => write!(f, "{}", quote(&c.to_string(), '\''))?,
            Kind::String(s) => write!(f, "{}", quote(s, '"'))?,
            Kind::Var(n) => write!(f, "{n}")?,
            Kind::Lam(n, body) => {
                write!(f, "\\{n} -> ")?;
                body.print(f, 0)?;
            }
            Kind::App(a, b) => {
                a.print(f, 7)?;
                write!(f, " ")?;
                b.print(f, 8)?;
            }
            Kind::LetGroup(bindings, body) => {
                write!(f, "let {{ ")?;
                for b in bindings {
                    write!(f, "{} = {}; ", b.name, b.rhs)?;
                }
                write!(f, "}} in {body}")?;
            }
            Kind::Let(n, a, b) => {
                write!(f, "let {{ {n} = ")?;
                a.print(f, 0)?;
                write!(f, "; }} in ")?;
                b.print(f, 0)?;
            }
            Kind::If(c, a, b) => {
                write!(f, "if ")?;
                c.print(f, 0)?;
                write!(f, " then ")?;
                a.print(f, 0)?;
                write!(f, " else ")?;
                b.print(f, 0)?;
            }
            Kind::Neg(a) => {
                write!(f, "-")?;
                a.print(f, 6)?;
            }
            Kind::Binary(op, a, b) => {
                // Comparisons are non-associative: parenthesize both nested sides.
                a.print(f, power + u8::from(power == 1))?;
                write!(f, " {} ", op.text())?;
                b.print(f, power + 1)?;
            }
            Kind::Chain(a, pairs, last) => {
                write!(f, "{a}")?;
                for (op, b) in pairs {
                    write!(f, " {op} {b}")?;
                }
                if let Some(op) = last {
                    write!(f, " {op}")?;
                }
            }
            Kind::Paren(a) => write!(f, "({a})")?,
            Kind::RightSection(op, a) => write!(f, "({op} {a})")?,
            Kind::Annotated(a, s) => write!(f, "({a} :: {s})")?,
            Kind::Record(n, fields) => {
                write!(f, "{n} {{")?;
                for (n, e) in fields {
                    write!(f, " {n} = {e},")?;
                }
                write!(f, " }}")?;
            }
            Kind::Project(e, n) => write!(f, "({e}).{n}")?,
            Kind::Update(e, fields) => {
                write!(f, "({e}) {{")?;
                for (n, e) in fields {
                    write!(f, " {n} = {e},")?;
                }
                write!(f, " }}")?;
            }
            Kind::ProjectionSection(n) => write!(f, "(.{n})")?,
            Kind::Equations(rows) => {
                write!(f, "equations {{ ")?;
                for row in rows {
                    write!(f, "{row}; ")?;
                }
                write!(f, "}}")?;
            }
            Kind::PatternLam(pats, body) => {
                write!(f, "\\")?;
                for p in pats {
                    write!(f, "{p} ")?;
                }
                write!(f, "-> {body}")?;
            }
            Kind::Case(scrut, rows) => {
                write!(f, "case {scrut} of {{ ")?;
                for row in rows {
                    write!(f, "{row}; ")?;
                }
                write!(f, "}}")?;
            }
            Kind::List(es) | Kind::Tuple(es) => {
                let list = matches!(&self.kind, Kind::List(_));
                write!(f, "{}", if list { "[" } else { "(" })?;
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, "{}", if list { "]" } else { ")" })?;
            }
            Kind::Error => write!(f, "<error>")?,
        }
        if parens {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f, 0)
    }
}

/// Collected equations or a generated strict pattern-binding component.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub nonrecursive: Vec<String>,
    pub name: String,
    pub rhs: Expr,
    pub span: Span,
}

/// A single source file with declarations in source order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub bindings: Vec<Binding>,
    pub fixities: Vec<Fixity>,
    pub data: Vec<DataDecl>,
    pub aliases: Vec<TypeAlias>,
    pub classes: Vec<ClassDecl>,
    pub instances: Vec<InstanceDecl>,
    pub span: Span,
}

impl Program {
    /// Preserve source order: each declaration scopes over only itself and later declarations.
    pub fn expression(&self) -> Expr {
        let mut body = Expr {
            kind: Kind::Var("main".into()),
            span: self.span,
        };
        for binding in self.bindings.iter().rev() {
            body = Expr {
                kind: Kind::Let(
                    binding.name.clone(),
                    Box::new(binding.rhs.clone()),
                    Box::new(body),
                ),
                span: binding.span,
            };
        }
        body
    }
}

/// Haskell-style quoted characters and primitive UTF-8 strings.
pub fn quote(text: &str, delimiter: char) -> String {
    let mut result = String::from(delimiter);
    for c in text.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            '\\' => result.push_str("\\\\"),
            c if c == delimiter => {
                result.push('\\');
                result.push(c);
            }
            c if c.is_control() => {
                result.push_str(&format!("\\u{{{:x}}}", c as u32))
            }
            c => result.push(c),
        }
    }
    result.push(delimiter);
    result
}

/// Free names account for lexical shadowing and recursive local binding scope.
pub fn free(expr: &Expr) -> std::collections::BTreeSet<String> {
    use std::collections::BTreeSet;
    match &expr.kind {
        Kind::Project(e, _) => free(e),
        Kind::ProjectionSection(_) => BTreeSet::new(),
        Kind::Record(_, fields) => {
            fields.iter().flat_map(|(_, e)| free(e)).collect()
        }
        Kind::Update(e, fields) => {
            let mut ns = free(e);
            ns.extend(fields.iter().flat_map(|(_, e)| free(e)));
            ns
        }
        Kind::Var(n) => BTreeSet::from([n.clone()]),
        Kind::List(es) | Kind::Tuple(es) => {
            es.iter().flat_map(free).collect()
        }
        Kind::Equations(rows) => rows.iter().flat_map(Row::free).collect(),
        Kind::Case(e, rows) => {
            let mut names = free(e);
            names.extend(rows.iter().flat_map(Row::free));
            names
        }
        Kind::PatternLam(pats, body) => {
            let mut names = free(body);
            for p in pats {
                for n in p.names() {
                    names.remove(&n);
                }
            }
            names
        }
        Kind::Lam(n, a) => {
            let mut names = free(a);
            names.remove(n);
            names
        }
        Kind::LetGroup(bindings, body) => {
            let mut names = free(body);
            for b in bindings {
                names.extend(free(&b.rhs));
            }
            for b in bindings {
                names.remove(&b.name);
            }
            names
        }
        Kind::Let(n, a, b) => {
            let mut names = free(a);
            names.extend(free(b));
            names.remove(n);
            names
        }
        Kind::App(a, b) | Kind::Binary(_, a, b) => {
            let mut names = free(a);
            names.extend(free(b));
            names
        }
        Kind::If(c, a, b) => {
            let mut names = free(c);
            names.extend(free(a));
            names.extend(free(b));
            names
        }
        Kind::Neg(a) | Kind::Annotated(a, _) | Kind::Paren(a) => free(a),
        Kind::Chain(a, pairs, last) => {
            let mut names = free(a);
            for (op, b) in pairs {
                names.insert(op.clone());
                names.extend(free(b));
            }
            if let Some(op) = last {
                names.insert(op.clone());
            }
            names
        }
        Kind::RightSection(op, a) => {
            let mut names = free(a);
            names.insert(op.clone());
            names
        }
        _ => BTreeSet::new(),
    }
}

/// Surface type syntax keeps names until a signature is closed implicitly.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSyntax {
    Qualified(Vec<PredicateSyntax>, Box<TypeSyntax>),
    Name(String),
    App(Box<TypeSyntax>, Box<TypeSyntax>),
    Arrow(Box<TypeSyntax>, Box<TypeSyntax>),
}

/// Declarations are collected before signatures are attached to their bindings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    Class(ClassDecl),
    Instance(InstanceDecl),
    Data(DataDecl),
    Alias(TypeAlias),
    Fixity(Fixity),
    Binding(Binding),
    Pattern(Pat, RowBody, Vec<Binding>, Span),
    Signature(Vec<String>, TypeSyntax, Span),
}

/// Associativity and precedence are resolved after the complete file is parsed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fixity {
    pub associativity: Associativity,
    pub precedence: u8,
    pub operators: Vec<String>,
    pub span: Span,
}

/// A nominal datatype with star-kinded parameters in this snapshot.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataDecl {
    pub name: String,
    pub parameters: Vec<String>,
    pub constructors: Vec<ConstructorDecl>,
    pub span: Span,
}

/// Positional fields are strict and ordered as declared.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstructorDecl {
    pub labels: Option<Vec<String>>,
    pub name: String,
    pub fields: Vec<TypeSyntax>,
    pub span: Span,
}

impl std::fmt::Display for TypeSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Qualified(ps, t) => {
                write!(f, "(")?;
                for (i, p) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} {}", p.class, p.variable)?;
                }
                write!(f, ") => {t}")
            }
            Self::Name(n) => write!(f, "{n}"),
            Self::App(a, b) => write!(f, "({a} {b})"),
            Self::Arrow(a, b) => write!(f, "({a} -> {b})"),
        }
    }
}

/// A source pattern retains its own diagnostic range, including nested nodes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// Constructor names remain unresolved until the renamer visits the pattern.
pub enum PatKind {
    Record(String, Vec<(String, Pat)>),
    Var(String),
    Wild,
    Con(String, Vec<Pat>),
    As(String, Box<Pat>),
    Double(u64),
    Int(i64),
    Char(char),
    String(String),
    List(Vec<Pat>),
    Tuple(Vec<Pat>),
}

/// Guards fall through to the next equation when every Boolean test fails.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RowBody {
    Plain(Box<Expr>),
    Guarded(Vec<(Expr, Expr)>),
}

/// Where declarations share one scope across all guards and their right-hand sides.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Row {
    pub patterns: Vec<Pat>,
    pub body: RowBody,
    pub locals: Vec<Binding>,
    pub span: Span,
}

impl Pat {
    pub fn names(&self) -> Vec<String> {
        match &self.kind {
            PatKind::Record(_, fields) => {
                fields.iter().flat_map(|(_, p)| p.names()).collect()
            }
            PatKind::Var(n) => vec![n.clone()],
            PatKind::As(n, p) => {
                let mut names = vec![n.clone()];
                names.extend(p.names());
                names
            }
            PatKind::Con(_, ps)
            | PatKind::List(ps)
            | PatKind::Tuple(ps) => {
                ps.iter().flat_map(Self::names).collect()
            }
            _ => vec![],
        }
    }
}

impl Row {
    fn free(&self) -> std::collections::BTreeSet<String> {
        let mut names = match &self.body {
            RowBody::Plain(e) => free(e),
            RowBody::Guarded(gs) => gs
                .iter()
                .flat_map(|(g, e)| {
                    let mut names = free(g);
                    names.extend(free(e));
                    names
                })
                .collect(),
        };
        for b in &self.locals {
            names.extend(free(&b.rhs));
        }
        for b in &self.locals {
            names.remove(&b.name);
        }
        for p in &self.patterns {
            for name in p.names() {
                names.remove(&name);
            }
        }
        names
    }
}

impl std::fmt::Display for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            PatKind::Record(n, fields) => {
                write!(f, "{n} {{")?;
                for (n, p) in fields {
                    write!(f, " {n} = {p},")?;
                }
                write!(f, " }}")
            }
            PatKind::Var(n) => write!(f, "{n}"),
            PatKind::Wild => write!(f, "_"),
            PatKind::Double(n) => write!(
                f,
                "{}",
                crate::primitives::show_double(f64::from_bits(*n))
            ),
            PatKind::Int(n) => write!(f, "{n}"),
            PatKind::Char(c) => {
                write!(f, "{}", quote(&c.to_string(), '\''))
            }
            PatKind::String(s) => write!(f, "{}", quote(s, '"')),
            PatKind::As(n, p) => write!(f, "{n}@{p}"),
            PatKind::Con(n, ps) => {
                write!(f, "({n}")?;
                for p in ps {
                    write!(f, " {p}")?;
                }
                write!(f, ")")
            }
            PatKind::List(ps) | PatKind::Tuple(ps) => {
                let list = matches!(&self.kind, PatKind::List(_));
                write!(f, "{}", if list { "[" } else { "(" })?;
                for (i, p) in ps.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{p}")?;
                }
                write!(f, "{}", if list { "]" } else { ")" })
            }
        }
    }
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in &self.patterns {
            write!(f, "{p} ")?;
        }
        match &self.body {
            RowBody::Plain(e) => write!(f, "-> {e}")?,
            RowBody::Guarded(gs) => {
                for (g, e) in gs {
                    write!(f, "| {g} -> {e} ")?;
                }
            }
        }
        if !self.locals.is_empty() {
            write!(f, " where {{ ")?;
            for b in &self.locals {
                write!(f, "{} = {}; ", b.name, b.rhs)?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for DataDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "data {}", self.name)?;
        for p in &self.parameters {
            write!(f, " {p}")?;
        }
        for (i, c) in self.constructors.iter().enumerate() {
            write!(f, " {} {}", if i == 0 { "=" } else { "|" }, c.name)?;
            if let Some(labels) = &c.labels {
                write!(f, " {{")?;
                for (i, (name, ty)) in
                    labels.iter().zip(&c.fields).enumerate()
                {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " {name} :: {ty}")?;
                }
                write!(f, " }}")?;
            } else {
                for t in &c.fields {
                    write!(f, " {t}")?;
                }
            }
        }
        Ok(())
    }
}

/// A transparent type abbreviation; applications are expanded before inference.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAlias {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: TypeSyntax,
    pub span: Span,
}

/// Source contexts constrain one type variable per predicate.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PredicateSyntax {
    pub class: String,
    pub variable: String,
}

/// Class signatures and defaults are checked together after all names are known.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassDecl {
    pub name: String,
    pub parameter: String,
    pub supers: Vec<PredicateSyntax>,
    pub members: Vec<Declaration>,
    pub span: Span,
}

/// An instance has one nominal head and ordinary method bindings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InstanceDecl {
    pub class: String,
    pub head: TypeSyntax,
    pub context: Vec<PredicateSyntax>,
    pub methods: Vec<Binding>,
    pub span: Span,
}

impl fmt::Display for TypeAlias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {}", self.name)?;
        for parameter in &self.parameters {
            write!(f, " {parameter}")?;
        }
        write!(f, " = {}", self.body)
    }
}

fn display_context(
    predicates: &[PredicateSyntax],
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    if predicates.is_empty() {
        return Ok(());
    }
    write!(f, "(")?;
    for (i, p) in predicates.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{} {}", p.class, p.variable)?;
    }
    write!(f, ") => ")
}

impl fmt::Display for ClassDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "class ")?;
        display_context(&self.supers, f)?;
        write!(f, "{} {} where {{", self.name, self.parameter)?;
        for member in &self.members {
            match member {
                Declaration::Signature(names, ty, _) => {
                    write!(f, " {} :: {ty};", names.join(", "))?
                }
                Declaration::Binding(b) => {
                    write!(f, " {} = {};", b.name, b.rhs)?
                }
                _ => {}
            }
        }
        write!(f, " }}")
    }
}

impl fmt::Display for InstanceDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "instance ")?;
        display_context(&self.context, f)?;
        write!(f, "{} ({}) where {{", self.class, self.head)?;
        for b in &self.methods {
            write!(f, " {} = {};", b.name, b.rhs)?;
        }
        write!(f, " }}")
    }
}
