//! Level-based HM. Metavariables are mutable cells; zonking compresses their solution paths.
use crate::{
    ast::{self, Op},
    diagnostics::{Diagnostic, ErrorCode},
    names::Id,
    renamed::{BindGroup, Expr, Kind, Pat, PatKind, RowBody},
    session::{Session, Stop},
    syn::Program,
    types::{self, Scheme, Type},
};
use std::collections::{BTreeMap, BTreeSet};

// ANCHOR: field-wanted
/// A field site's position distinguishes updates of several named slots.
pub type FieldSite = (u32, usize);

/// Field constraints never enter a polymorphic scheme.
#[derive(Clone)]
struct FieldWanted {
    record: Type,
    label: String,
    field: Type,
    site: FieldSite,
    span: crate::source::Span,
    level: u32,
}
// ANCHOR_END: field-wanted

// ANCHOR: store
#[derive(Clone)]
struct Meta {
    level: u32,
    solution: Option<Type>,
}

#[derive(Clone, Default)]
struct Infer {
    wanted: Vec<FieldWanted>,
    resolved: BTreeMap<FieldSite, (crate::data::ConId, usize)>,
    level: u32,
    data: crate::data::DataEnv,
    metas: Vec<Meta>,
    instantiations: BTreeMap<u32, Vec<Type>>,
    schemes: BTreeMap<Id, Scheme>,
    parameters: BTreeMap<Id, Type>,
    expression_types: BTreeMap<u32, Type>,
}

impl Infer {
    fn field(
        &mut self,
        record: Type,
        label: String,
        field: Type,
        site: FieldSite,
        expr: &Expr,
    ) -> Result<(), Diagnostic> {
        self.wanted.push(FieldWanted {
            record,
            label,
            field,
            site,
            span: expr.span,
            level: self.level,
        });
        self.solve_fields(None)
    }
    fn solve_field(
        &mut self,
        wanted: &FieldWanted,
    ) -> Result<bool, Diagnostic> {
        let record = self.zonk(&wanted.record);
        let candidates = self
            .data
            .constructors
            .iter()
            .filter(|c| {
                c.labels
                    .as_ref()
                    .is_some_and(|ls| ls.contains(&wanted.label))
            })
            .cloned()
            .collect::<Vec<_>>();
        let con = if candidates.len() == 1 {
            Some(candidates[0].clone())
        } else {
            match &record {
                Type::Var(_) => None,
                Type::Con(id, _) => self
                    .data
                    .constructors
                    .iter()
                    .find(|c| &c.parent == id && c.labels.is_some())
                    .cloned(),
                _ => None,
            }
        };
        let Some(con) = con else {
            if !matches!(record, Type::Var(_)) {
                return Err(Diagnostic::error(
                    ErrorCode::NoField,
                    format!("type {record} has no field {}", wanted.label),
                    wanted.span,
                ));
            }
            return Ok(false);
        };
        let index = con
            .labels
            .as_ref()
            .unwrap()
            .iter()
            .position(|n| n == &wanted.label)
            .ok_or_else(|| {
                Diagnostic::error(
                    ErrorCode::NoField,
                    format!(
                        "type {} has no field {}",
                        con.parent.name, wanted.label
                    ),
                    wanted.span,
                )
            })?;
        let mut ty = self.instantiate(&con.scheme);
        let mut fields = vec![];
        for _ in &con.fields {
            let Type::Arrow(a, b) = ty else {
                unreachable!()
            };
            fields.push(*a);
            ty = *b;
        }
        let site = Expr {
            node: wanted.site.0,
            kind: Kind::Int(0),
            span: wanted.span,
        };
        self.unify(&wanted.record, &ty, &site)?;
        self.unify(&wanted.field, &fields[index], &site)?;
        self.resolved.insert(wanted.site, (con.id, index));
        Ok(true)
    }
    // ANCHOR: field-solver
    fn solve_fields(
        &mut self,
        complete: Option<u32>,
    ) -> Result<(), Diagnostic> {
        loop {
            let pending = std::mem::take(&mut self.wanted);
            let count = pending.len();
            for wanted in pending {
                if !self.solve_field(&wanted)? {
                    self.wanted.push(wanted);
                }
            }
            if self.wanted.len() == count {
                break;
            }
        }
        if let Some(level) = complete
            && let Some(w) = self.wanted.iter().find(|w| w.level > level)
        {
            let mut error = Diagnostic::error(
                ErrorCode::AmbiguousField,
                format!("ambiguous field access .{}", w.label),
                w.span,
            );
            error.notes.push(
                "add a type annotation to identify the record type".into(),
            );
            return Err(error);
        }
        Ok(())
    }
    // ANCHOR_END: field-solver
    fn infer_expected(
        &mut self,
        env: &mut Vec<(Id, Scheme)>,
        expr: &Expr,
        expected: &Type,
    ) -> Result<Type, Diagnostic> {
        if let Kind::Lam(id, body) = &expr.kind {
            let input = self.fresh();
            let output = self.fresh();
            self.unify(
                expected,
                &Type::arrow(input.clone(), output.clone()),
                expr,
            )?;
            self.parameters.insert(id.clone(), input.clone());
            env.push((id.clone(), Scheme::mono(input.clone())));
            let result = self.infer_expected(env, body, &output);
            env.pop();
            let ty = Type::arrow(input, result?);
            self.expression_types.insert(expr.node, ty.clone());
            Ok(ty)
        } else {
            let ty = self.infer(env, expr)?;
            self.unify(&ty, expected, expr)?;
            Ok(ty)
        }
    }
    fn fresh(&mut self) -> Type {
        let n = self.metas.len() as u32;
        self.metas.push(Meta {
            level: self.level,
            solution: None,
        });
        Type::Var(n)
    }
    fn zonk(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Var(n) => {
                match self.metas[*n as usize].solution.clone() {
                    None => ty.clone(),
                    Some(t) => {
                        let t = self.zonk(&t);
                        self.metas[*n as usize].solution = Some(t.clone());
                        t
                    }
                }
            }
            Type::Con(id, args) => Type::Con(
                id.clone(),
                args.iter().map(|a| self.zonk(a)).collect(),
            ),
            Type::Arrow(a, b) => Type::arrow(self.zonk(a), self.zonk(b)),
            other => other.clone(),
        }
    }
    // ANCHOR_END: store
    // ANCHOR: levels
    /// One traversal rejects cycles and prevents younger variables escaping their scope.
    fn occurs_and_lower(
        &mut self,
        n: u32,
        level: u32,
        ty: &Type,
        expr: &Expr,
    ) -> Result<(), Diagnostic> {
        match self.zonk(ty) {
            Type::Var(m) if m == n => Err(types::occurs(expr)),
            Type::Var(m) => {
                self.metas[m as usize].level =
                    self.metas[m as usize].level.min(level);
                Ok(())
            }
            Type::Con(_, args) => {
                for a in args {
                    self.occurs_and_lower(n, level, &a, expr)?;
                }
                Ok(())
            }
            Type::Arrow(a, b) => {
                self.occurs_and_lower(n, level, &a, expr)?;
                self.occurs_and_lower(n, level, &b, expr)
            }
            _ => Ok(()),
        }
    }
    fn unify(
        &mut self,
        a: &Type,
        b: &Type,
        expr: &Expr,
    ) -> Result<(), Diagnostic> {
        let (a, b) = (self.zonk(a), self.zonk(b));
        match (&a, &b) {
            _ if a == b => Ok(()),
            (Type::Var(n), t) | (t, Type::Var(n)) => {
                self.occurs_and_lower(
                    *n,
                    self.metas[*n as usize].level,
                    t,
                    expr,
                )?;
                self.metas[*n as usize].solution = Some(t.clone());
                Ok(())
            }
            (Type::Con(a, xs), Type::Con(b, ys))
                if a == b && xs.len() == ys.len() =>
            {
                for (x, y) in xs.iter().zip(ys) {
                    self.unify(x, y, expr)?;
                }
                Ok(())
            }
            (Type::Arrow(a, b), Type::Arrow(c, d)) => {
                self.unify(a, c, expr)?;
                self.unify(b, d, expr)
            }
            _ => Err(types::mismatch(expr, &a, &b)),
        }
    }
    // ANCHOR_END: levels
    fn generalize(&mut self, ty: Type) -> Scheme {
        fn visit(
            ty: &Type,
            metas: &[Meta],
            level: u32,
            vars: &mut BTreeSet<u32>,
        ) {
            match ty {
                Type::Var(n) if metas[*n as usize].level > level => {
                    vars.insert(*n);
                }
                Type::Con(_, args) => {
                    for a in args {
                        visit(a, metas, level, vars);
                    }
                }
                Type::Arrow(a, b) => {
                    visit(a, metas, level, vars);
                    visit(b, metas, level, vars);
                }
                _ => {}
            }
        }
        let ty = self.zonk(&ty);
        let mut vars = BTreeSet::new();
        visit(&ty, &self.metas, self.level, &mut vars);
        Scheme {
            variables: vars.into_iter().collect(),
            ty,
        }
    }
    fn instantiate(&mut self, scheme: &Scheme) -> Type {
        self.instantiate_with_arguments(scheme).0
    }
    fn instantiate_with_arguments(
        &mut self,
        scheme: &Scheme,
    ) -> (Type, Vec<Type>) {
        fn replace(ty: &Type, mapping: &BTreeMap<u32, Type>) -> Type {
            match ty {
                Type::Var(n) => {
                    mapping.get(n).cloned().unwrap_or_else(|| ty.clone())
                }
                Type::Con(id, args) => Type::Con(
                    id.clone(),
                    args.iter().map(|a| replace(a, mapping)).collect(),
                ),
                Type::Arrow(a, b) => {
                    Type::arrow(replace(a, mapping), replace(b, mapping))
                }
                other => other.clone(),
            }
        }
        let arguments: Vec<_> =
            scheme.variables.iter().map(|_| self.fresh()).collect();
        let mapping = scheme
            .variables
            .iter()
            .copied()
            .zip(arguments.iter().cloned())
            .collect();
        (replace(&scheme.ty, &mapping), arguments)
    }
    /// Instantiate the inferred scheme against rigid signature variables.
    /// A trial store keeps skolems out of the live inference state.
    // ANCHOR: signature
    fn check_signature(
        &mut self,
        inferred: &Scheme,
        declared: &Scheme,
        expr: &Expr,
    ) -> Result<Scheme, Diagnostic> {
        fn rigid(ty: &Type) -> Type {
            match ty {
                Type::Var(n) => Type::Skolem(*n),
                Type::Con(id, args) => {
                    Type::Con(id.clone(), args.iter().map(rigid).collect())
                }
                Type::Arrow(a, b) => Type::arrow(rigid(a), rigid(b)),
                t => t.clone(),
            }
        }
        fn has_skolem(ty: &Type) -> bool {
            match ty {
                Type::Skolem(_) => true,
                Type::Con(_, args) => args.iter().any(has_skolem),
                Type::Arrow(a, b) => has_skolem(a) || has_skolem(b),
                _ => false,
            }
        }
        let mut trial = self.clone();
        let actual = trial.instantiate(inferred);
        let expected = rigid(&declared.ty);
        let invalid = trial.unify(&actual, &expected, expr).is_err()
            || (0..self.metas.len()).any(|n| {
                self.metas[n].level <= self.level
                    && has_skolem(&trial.zonk(&Type::Var(n as u32)))
            });
        if invalid {
            return Err(Diagnostic::error(
                ErrorCode::Signature,
                format!(
                    "signature {} is not an instance of inferred type {}",
                    declared.ty,
                    inferred.ty.canonical()
                ),
                expr.span,
            ));
        }
        // Specialization can constrain variables captured from an outer scope.
        self.level += 1;
        let actual = self.instantiate(inferred);
        let expected = self.instantiate(declared);
        let result = self.unify(&actual, &expected, expr);
        self.level -= 1;
        result?;
        Ok(self.generalize(expected))
    }
    // ANCHOR_END: signature
    /// A recursive component shares monomorphic provisional types during inference.
    // ANCHOR: group
    fn group(
        &mut self,
        env: &mut Vec<(Id, Scheme)>,
        group: &BindGroup,
    ) -> Result<(), Diagnostic> {
        self.level += 1;
        let size = env.len();
        let provisional: Vec<_> = group
            .binds
            .iter()
            .map(|b| (b.id.clone(), self.fresh()))
            .collect();
        if group.recursive {
            env.extend(
                provisional
                    .iter()
                    .map(|(id, ty)| (id.clone(), Scheme::mono(ty.clone()))),
            );
        }
        let inferred = group
            .binds
            .iter()
            .zip(&provisional)
            .map(|(b, (_, expected))| {
                let ty = if let Some(annotation) = &b.annotation {
                    let declared =
                        self.data.signature(annotation, b.span)?;
                    let expected = self.instantiate(&declared);
                    self.infer_expected(env, &b.rhs, &expected).map_err(
                        |mut error| {
                            if error.code == ErrorCode::Mismatch {
                                error.code = ErrorCode::Signature;
                            }
                            error
                        },
                    )?
                } else {
                    self.infer(env, &b.rhs)?
                };
                if group.recursive {
                    self.unify(expected, &ty, &b.rhs)?;
                }
                Ok(ty)
            })
            .collect::<Result<Vec<_>, Diagnostic>>();
        env.truncate(size);
        self.level -= 1;
        let inferred = inferred?;
        self.solve_fields(None)?;
        for (binding, ty) in group.binds.iter().zip(&inferred) {
            if let Some(annotation) = &binding.annotation {
                let scheme = self.generalize(ty.clone());
                let annotation =
                    self.data.signature(annotation, binding.span)?;
                self.check_signature(&scheme, &annotation, &binding.rhs)?;
            }
        }
        self.level += 1;
        for (binding, ty) in group.binds.iter().zip(&inferred) {
            if let Some(annotation) = &binding.annotation {
                let annotation =
                    self.data.signature(annotation, binding.span)?;
                let declared = self.instantiate(&annotation);
                self.unify(ty, &declared, &binding.rhs)?;
            }
        }
        self.level -= 1;
        self.solve_fields(Some(self.level))?;
        for (binding, ty) in group.binds.iter().zip(inferred) {
            let scheme = self.generalize(ty);
            self.schemes.insert(binding.id.clone(), scheme.clone());
            env.push((binding.id.clone(), scheme));
        }
        Ok(())
    }
    // ANCHOR_END: group
    fn pattern(
        &mut self,
        env: &mut Vec<(Id, Scheme)>,
        pattern: &Pat,
        expected: &Type,
    ) -> Result<(), Diagnostic> {
        let site = Expr {
            node: u32::MAX,
            kind: Kind::Int(0),
            span: pattern.span,
        };
        match &pattern.kind {
            PatKind::Var(id) => {
                self.parameters.insert(id.clone(), expected.clone());
                env.push((id.clone(), Scheme::mono(expected.clone())));
            }
            PatKind::As(id, p) => {
                self.parameters.insert(id.clone(), expected.clone());
                env.push((id.clone(), Scheme::mono(expected.clone())));
                self.pattern(env, p, expected)?;
            }
            PatKind::Wild => {}
            PatKind::Int(_) => self.unify(expected, &Type::Int, &site)?,
            PatKind::Char(_) => self.unify(expected, &Type::Char, &site)?,
            PatKind::String(_) => {
                self.unify(expected, &Type::String, &site)?
            }
            // ANCHOR: constructor-pattern
            PatKind::Con(id, patterns) => {
                let info = self.data.con(id).clone();
                if patterns.len() != info.fields.len() {
                    return Err(Diagnostic::error(
                        ErrorCode::Mismatch,
                        format!(
                            "{} expects {} pattern fields",
                            id.name,
                            info.fields.len()
                        ),
                        pattern.span,
                    ));
                }
                let mut ty = self.instantiate(&info.scheme);
                let mut fields = Vec::new();
                for _ in patterns {
                    let Type::Arrow(a, b) = ty else {
                        unreachable!("constructor arity")
                    };
                    fields.push(*a);
                    ty = *b;
                }
                self.unify(expected, &ty, &site)?;
                for (pattern, ty) in patterns.iter().zip(fields) {
                    self.pattern(env, pattern, &ty)?;
                }
            }
            // ANCHOR_END: constructor-pattern
            PatKind::List(..) | PatKind::Tuple(..) => {
                unreachable!("pattern sugar must be lowered")
            }
        }
        Ok(())
    }
    fn infer(
        &mut self,
        env: &mut Vec<(Id, Scheme)>,
        expr: &Expr,
    ) -> Result<Type, Diagnostic> {
        let result = match &expr.kind {
            Kind::Annotated(e, syntax) => {
                let scheme = self.data.signature(syntax, expr.span)?;
                self.level += 1;
                let expected = self.instantiate(&scheme);
                let actual = self.infer_expected(env, e, &expected);
                self.level -= 1;
                let actual = actual?;
                self.solve_fields(None)?;
                let inferred = self.generalize(actual.clone());
                self.check_signature(&inferred, &scheme, expr)?;
                Ok(actual)
            }
            Kind::Record(id, fields) => {
                let names = fields
                    .iter()
                    .map(|(n, _)| n.clone())
                    .collect::<Vec<_>>();
                let indices =
                    self.data.fields(id, &names, true, expr.span)?;
                let info = self.data.con(id).clone();
                let (mut ty, args) =
                    self.instantiate_with_arguments(&info.scheme);
                self.instantiations.insert(expr.node, args);
                let mut types = vec![];
                for _ in &info.fields {
                    let Type::Arrow(a, b) = ty else {
                        unreachable!()
                    };
                    types.push(*a);
                    ty = *b;
                }
                for ((_, e), index) in fields.iter().zip(indices) {
                    let actual = self.infer(env, e)?;
                    self.unify(&actual, &types[index], e)?;
                }
                Ok(ty)
            }
            // ANCHOR: field-wanted-generation
            Kind::Project(e, label) => {
                let record = self.infer(env, e)?;
                let field = self.fresh();
                self.field(
                    record,
                    label.clone(),
                    field.clone(),
                    (expr.node, 0),
                    expr,
                )?;
                Ok(field)
            }
            // ANCHOR_END: field-wanted-generation
            Kind::Update(e, fields) => {
                let record = self.infer(env, e)?;
                let mut seen = BTreeSet::new();
                for (i, (label, value)) in fields.iter().enumerate() {
                    if !seen.insert(label) {
                        return Err(Diagnostic::error(
                            ErrorCode::RepeatedField,
                            format!("duplicate field {label}"),
                            expr.span,
                        ));
                    }
                    let ty = self.infer(env, value)?;
                    self.field(
                        record.clone(),
                        label.clone(),
                        ty,
                        (expr.node, i),
                        expr,
                    )?;
                }
                Ok(record)
            }
            Kind::Con(id) => {
                let scheme = self.data.con(id).scheme.clone();
                let (ty, args) = self.instantiate_with_arguments(&scheme);
                self.instantiations.insert(expr.node, args);
                Ok(ty)
            }
            Kind::Match(scruts, rows) => {
                let types = scruts
                    .iter()
                    .map(|e| self.infer(env, e))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.fresh();
                for row in rows {
                    if row.patterns.len() != types.len() {
                        return Err(Diagnostic::error(
                            ErrorCode::Mismatch,
                            "pattern matrix arity mismatch",
                            row.span,
                        ));
                    }
                    let size = env.len();
                    for (pat, ty) in row.patterns.iter().zip(&types) {
                        self.pattern(env, pat, ty)?;
                    }
                    for group in &row.locals {
                        self.group(env, group)?;
                    }
                    match &row.body {
                        RowBody::Plain(body) => {
                            let ty = self.infer(env, body)?;
                            self.unify(&ty, &result, body)?;
                        }
                        RowBody::Guarded(gs) => {
                            for (guard, body) in gs {
                                let guard_ty = self.infer(env, guard)?;
                                self.unify(&guard_ty, &Type::Bool, guard)?;
                                let ty = self.infer(env, body)?;
                                self.unify(&ty, &result, body)?;
                            }
                        }
                    }
                    env.truncate(size);
                }
                Ok(result)
            }
            Kind::PatternLam(..)
            | Kind::Equations(..)
            | Kind::Case(..)
            | Kind::List(..)
            | Kind::Tuple(..) => {
                unreachable!("surface sugar must be lowered before typing")
            }
            Kind::Int(_) => Ok(Type::Int),
            Kind::Bool(_) => Ok(Type::Bool),
            Kind::Char(_) => Ok(Type::Char),
            Kind::String(_) => Ok(Type::String),
            Kind::Var(n) => {
                if n.text.starts_with("prim_") {
                    return Err(Diagnostic::error(
                        ErrorCode::PrivatePrimitive,
                        "primitive names are private to the Prelude",
                        expr.span,
                    ));
                }
                let scheme = env
                    .iter()
                    .rev()
                    .find(|(name, _)| name == n)
                    .map(|(_, s)| s.clone())
                    .ok_or_else(|| {
                        Diagnostic::error(
                            if n.text.starts_with(char::is_uppercase) {
                                ErrorCode::UnboundConstructor
                            } else {
                                ErrorCode::Unbound
                            },
                            format!("unbound variable {n}"),
                            expr.span,
                        )
                    })?;
                let (ty, arguments) =
                    self.instantiate_with_arguments(&scheme);
                self.instantiations.insert(expr.node, arguments);
                Ok(ty)
            }
            Kind::Lam(n, body) => {
                if n.text.starts_with("prim_") {
                    return Err(Diagnostic::error(
                        ErrorCode::PrivatePrimitive,
                        "primitive names are private to the Prelude",
                        expr.span,
                    ));
                }
                let a = self.fresh();
                self.parameters.insert(n.clone(), a.clone());
                env.push((n.clone(), Scheme::mono(a.clone())));
                let result = self.infer(env, body);
                env.pop();
                Ok(Type::arrow(a, result?))
            }
            Kind::App(f, a) => {
                let tf = self.infer(env, f)?;
                let ta = self.infer(env, a)?;
                let result = self.fresh();
                self.unify(&tf, &Type::arrow(ta, result.clone()), expr)?;
                Ok(result)
            }
            // ANCHOR: let
            Kind::Let(groups, body) => {
                let size = env.len();
                let result = (|| {
                    for group in groups {
                        self.group(env, group)?;
                    }
                    self.infer(env, body)
                })();
                env.truncate(size);
                result
            }
            // ANCHOR_END: let
            Kind::If(c, a, b) => {
                let tc = self.infer(env, c)?;
                self.unify(&tc, &Type::Bool, c)?;
                let ta = self.infer(env, a)?;
                let tb = self.infer(env, b)?;
                self.unify(&ta, &tb, b)?;
                Ok(ta)
            }
            Kind::Binary(op, a, b) => {
                if matches!(op, Op::Eq | Op::Lt | Op::Le)
                    && ["True", "False"].iter().any(|name| {
                        self.data
                            .constructor(name)
                            .is_none_or(|c| c.parent.name != "Bool")
                    })
                {
                    return Err(Diagnostic::error(
                        ErrorCode::UnboundConstructor,
                        "integer comparison requires the Prelude Bool declaration",
                        expr.span,
                    ));
                }
                let ta = self.infer(env, a)?;
                let operand = if *op == Op::Append {
                    Type::String
                } else {
                    Type::Int
                };
                self.unify(&ta, &operand, a)?;
                let tb = self.infer(env, b)?;
                self.unify(&tb, &operand, b)?;
                Ok(match op {
                    Op::Eq | Op::Lt | Op::Le => Type::Bool,
                    Op::Append => Type::String,
                    _ => Type::Int,
                })
            }
            Kind::Neg(a) => {
                let t = self.infer(env, a)?;
                self.unify(&t, &Type::Int, a)?;
                Ok(Type::Int)
            }
        };
        if let Ok(ty) = &result {
            self.expression_types.insert(expr.node, ty.clone());
        }
        result
    }
}

/// Typed Syn retains immutable, zonked metadata for every elaborated occurrence.
pub struct TypedProgram {
    pub resolved: BTreeMap<FieldSite, (crate::data::ConId, usize)>,
    pub surface: Program,
    pub main_type: Type,
    pub instantiations: BTreeMap<u32, Vec<Type>>,
    pub schemes: BTreeMap<Id, Scheme>,
    pub parameters: BTreeMap<Id, Type>,
    pub expression_types: BTreeMap<u32, Type>,
}

/// Check dependency-ordered groups; each recursive component generalizes together.
pub fn check(
    sess: &mut Session,
    program: ast::Program,
) -> Result<TypedProgram, Stop> {
    let program = crate::rename::rename(sess, program)?;
    check_renamed(sess, program)
}

/// Type inference consumes only unique identifiers and explicit dependency groups.
pub fn check_renamed(
    sess: &mut Session,
    program: crate::renamed::Program,
) -> Result<TypedProgram, Stop> {
    let syn = crate::desugar::desugar(sess, program)?;
    check_syn(sess, syn)
}

/// Check the desugared, scoped program and record its elaboration evidence.
pub fn check_syn(
    sess: &mut Session,
    program: Program,
) -> Result<TypedProgram, Stop> {
    let mut infer = Infer {
        data: program.data.clone(),
        level: 1,
        metas: vec![],
        ..Infer::default()
    };
    let mut env = Vec::new();
    for (op, id) in &program.primitives {
        let operand = if *op == Op::Append {
            Type::String
        } else {
            Type::Int
        };
        let result = if matches!(op, Op::Eq | Op::Lt | Op::Le) {
            Type::Bool
        } else {
            operand.clone()
        };
        env.push((
            id.clone(),
            Scheme::mono(Type::arrow(
                operand.clone(),
                Type::arrow(operand, result),
            )),
        ));
    }
    let result = (|| {
        for group in &program.groups {
            infer.group(&mut env, group)?;
        }
        infer.infer(
            &mut env,
            &Expr {
                node: u32::MAX,
                kind: Kind::Var(program.main.clone()),
                span: program.span,
            },
        )
    })();
    match result {
        Ok(ty) => {
            let main_type = infer.zonk(&ty).canonical();
            let instantiations = std::mem::take(&mut infer.instantiations)
                .into_iter()
                .map(|(n, args)| {
                    (n, args.iter().map(|ty| infer.zonk(ty)).collect())
                })
                .collect();
            let parameters = std::mem::take(&mut infer.parameters)
                .into_iter()
                .map(|(id, ty)| (id, infer.zonk(&ty)))
                .collect();
            let schemes = std::mem::take(&mut infer.schemes)
                .into_iter()
                .map(|(id, s)| {
                    (
                        id,
                        Scheme {
                            variables: s.variables,
                            ty: infer.zonk(&s.ty),
                        },
                    )
                })
                .collect();
            let expression_types =
                std::mem::take(&mut infer.expression_types)
                    .into_iter()
                    .map(|(node, ty)| (node, infer.zonk(&ty)))
                    .collect();
            Ok(TypedProgram {
                resolved: infer.resolved,
                expression_types,
                surface: program,
                main_type,
                instantiations,
                parameters,
                schemes,
            })
        }
        Err(error) => {
            sess.emit(error);
            Err(Stop)
        }
    }
}
