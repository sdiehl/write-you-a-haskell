//! Level-based HM. Metavariables are mutable cells; zonking compresses their solution paths.
use crate::{
    ast::{self, Op},
    diagnostics::{Diagnostic, ErrorCode},
    names::Id,
    renamed::{BindGroup, Expr, Kind, Pat, PatKind, RowBody},
    session::{Session, Stop},
    syn::Program,
    types::{self, Predicate, Scheme, Type},
};
use std::collections::{BTreeMap, BTreeSet};
/// Indexed lookup with an undo log keeps lexical scope entry and exit cheap.
#[derive(Default)]
struct Scope {
    values: BTreeMap<Id, Scheme>,
    undo: Vec<(Id, Option<Scheme>)>,
}
impl Scope {
    fn len(&self) -> usize {
        self.undo.len()
    }
    fn push(&mut self, (id, scheme): (Id, Scheme)) {
        let previous = self.values.insert(id.clone(), scheme);
        self.undo.push((id, previous));
    }
    fn pop(&mut self) {
        if let Some((id, old)) = self.undo.pop() {
            match old {
                Some(scheme) => {
                    self.values.insert(id, scheme);
                }
                None => {
                    self.values.remove(&id);
                }
            }
        }
    }
    fn truncate(&mut self, size: usize) {
        while self.len() > size {
            self.pop();
        }
    }
}
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
    kind: crate::kinds::Kind,
    level: u32,
    solution: Option<Type>,
}
#[derive(Clone, Default)]
struct Infer {
    required_main: Option<Id>,
    prelude_file: Option<crate::source::FileId>,
    classes: crate::classes::Env,
    class_wanted: Vec<(Predicate, crate::source::Span)>,
    occurrence_predicates: BTreeMap<u32, Vec<Predicate>>,
    skolem_kinds: BTreeMap<u32, crate::kinds::Kind>,
    wanted: Vec<FieldWanted>,
    resolved: BTreeMap<FieldSite, (crate::data::ConId, usize)>,
    level: u32,
    data: std::rc::Rc<crate::data::DataEnv>,
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
        self.unify_at(&wanted.record, &ty, wanted.span)?;
        self.unify_at(&wanted.field, &fields[index], wanted.span)?;
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
        env: &mut Scope,
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
    fn zonk_predicate(&mut self, p: &Predicate) -> Predicate {
        Predicate {
            class: p.class.clone(),
            ty: self.zonk(&p.ty),
        }
    }
    fn predicate_arguments(
        scheme: &Scheme,
        arguments: &[Type],
    ) -> Vec<Predicate> {
        let map = scheme
            .variables
            .iter()
            .copied()
            .zip(arguments.iter().cloned())
            .collect();
        scheme.predicates.iter().map(|p| p.replace(&map)).collect()
    }
    fn fresh(&mut self) -> Type {
        self.fresh_kind(crate::kinds::Kind::Star)
    }
    fn fresh_kind(&mut self, kind: crate::kinds::Kind) -> Type {
        let n = self.metas.len() as u32;
        self.metas.push(Meta {
            kind,
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
            Type::App(a, b) => Type::apply(self.zonk(a), self.zonk(b)),
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
        span: crate::source::Span,
    ) -> Result<(), Diagnostic> {
        match self.zonk(ty) {
            Type::Var(m) if m == n => Err(types::occurs_at(span)),
            Type::Var(m) => {
                self.metas[m as usize].level =
                    self.metas[m as usize].level.min(level);
                Ok(())
            }
            Type::Con(_, args) => {
                for a in args {
                    self.occurs_and_lower(n, level, &a, span)?;
                }
                Ok(())
            }
            Type::Arrow(a, b) | Type::App(a, b) => {
                self.occurs_and_lower(n, level, &a, span)?;
                self.occurs_and_lower(n, level, &b, span)
            }
            _ => Ok(()),
        }
    }
    fn type_kind(&self, t: &Type) -> crate::kinds::Kind {
        use crate::kinds::Kind as K;
        match t {
            Type::Var(n) => self.metas[*n as usize].kind.clone(),
            Type::Skolem(n) => self.skolem_kinds[n].clone(),
            Type::Con(id, args) => {
                let mut k = self.data.kinds[&id.name].clone();
                for _ in args {
                    let K::Arrow(_, b) = k else { unreachable!() };
                    k = *b;
                }
                k
            }
            Type::App(f, _) => {
                let K::Arrow(_, b) = self.type_kind(f) else {
                    unreachable!()
                };
                *b
            }
            _ => K::Star,
        }
    }
    fn unify_at(
        &mut self,
        a: &Type,
        b: &Type,
        span: crate::source::Span,
    ) -> Result<(), Diagnostic> {
        let (mut a, mut b) = (self.zonk(a), self.zonk(b));
        // Only the embedded Prelude may construct and run the IO representation.
        if self.prelude_file == Some(span.file) {
            let reveal = |t: Type| match t {
                Type::Con(id, args)
                    if id.name == "IO" && args.len() == 1 =>
                {
                    Type::arrow(
                        Type::Con(
                            self.data.tycon("()").unwrap().id.clone(),
                            vec![],
                        ),
                        args[0].clone(),
                    )
                }
                t => t,
            };
            // Keep nominal IO solutions when unifying an unconstrained variable.
            if matches!(b, Type::Arrow(..)) {
                a = reveal(a);
            }
            if matches!(a, Type::Arrow(..)) {
                b = reveal(b);
            }
        }
        if self.type_kind(&a) != self.type_kind(&b) {
            return Err(Diagnostic::error(
                ErrorCode::Kind,
                "type application kind mismatch",
                span,
            ));
        }
        match (&a, &b) {
            _ if a == b => Ok(()),
            (Type::Var(n), t) | (t, Type::Var(n)) => {
                self.occurs_and_lower(
                    *n,
                    self.metas[*n as usize].level,
                    t,
                    span,
                )?;
                self.metas[*n as usize].solution = Some(t.clone());
                Ok(())
            }
            (Type::Con(a, xs), Type::Con(b, ys))
                if a == b && xs.len() == ys.len() =>
            {
                for (x, y) in xs.iter().zip(ys) {
                    self.unify_at(x, y, span)?;
                }
                Ok(())
            }
            (Type::Arrow(a, b), Type::Arrow(c, d)) => {
                self.unify_at(a, c, span)?;
                self.unify_at(b, d, span)
            }
            (Type::App(f, x), t) | (t, Type::App(f, x)) => {
                let (g, y) = match t {
                    Type::App(g, y) => (*g.clone(), *y.clone()),
                    Type::Con(id, args) if !args.is_empty() => (
                        Type::Con(
                            id.clone(),
                            args[..args.len() - 1].to_vec(),
                        ),
                        args.last().unwrap().clone(),
                    ),
                    Type::Arrow(a, b) => (
                        Type::Con(
                            self.data
                                .tycon("(->)")
                                .expect("built-in arrow")
                                .id
                                .clone(),
                            vec![*a.clone()],
                        ),
                        *b.clone(),
                    ),
                    _ => return Err(types::mismatch_at(span, &a, &b)),
                };
                self.unify_at(f, &g, span)?;
                self.unify_at(x, &y, span)
            }
            _ => Err(types::mismatch_at(span, &a, &b)),
        }
    }
    fn unify(
        &mut self,
        a: &Type,
        b: &Type,
        expr: &Expr,
    ) -> Result<(), Diagnostic> {
        self.unify_at(a, b, expr.span)
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
                Type::Arrow(a, b) | Type::App(a, b) => {
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
            predicates: vec![],
            kinds: vars
                .iter()
                .map(|n| self.metas[*n as usize].kind.clone())
                .collect(),
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
                Type::App(a, b) => {
                    Type::apply(replace(a, mapping), replace(b, mapping))
                }
                Type::Arrow(a, b) => {
                    Type::arrow(replace(a, mapping), replace(b, mapping))
                }
                other => other.clone(),
            }
        }
        let arguments: Vec<_> = scheme
            .kinds
            .iter()
            .map(|k| self.fresh_kind(k.clone()))
            .collect();
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
                Type::App(a, b) => Type::apply(rigid(a), rigid(b)),
                Type::Arrow(a, b) => Type::arrow(rigid(a), rigid(b)),
                t => t.clone(),
            }
        }
        fn has_skolem(ty: &Type) -> bool {
            match ty {
                Type::Skolem(_) => true,
                Type::Con(_, args) => args.iter().any(has_skolem),
                Type::Arrow(a, b) | Type::App(a, b) => {
                    has_skolem(a) || has_skolem(b)
                }
                _ => false,
            }
        }
        // Only cells reachable from the inferred type can be constrained by
        // this trial. A compact store avoids cloning all earlier declarations.
        let variables = inferred.ty.variables();
        let mapping = variables
            .iter()
            .enumerate()
            .map(|(i, n)| (*n, Type::Var(i as u32)))
            .collect::<BTreeMap<_, _>>();
        let mut compact = inferred.clone();
        compact.ty = compact.ty.replace(&mapping);
        compact.variables = compact
            .variables
            .iter()
            .filter_map(|n| match mapping.get(n) {
                Some(Type::Var(i)) => Some(*i),
                _ => None,
            })
            .collect();
        compact.kinds = inferred
            .variables
            .iter()
            .zip(&inferred.kinds)
            .filter(|(n, _)| mapping.contains_key(n))
            .map(|(_, k)| k.clone())
            .collect();
        let mut trial = Self {
            prelude_file: self.prelude_file,
            level: self.level,
            metas: variables
                .iter()
                .map(|n| {
                    let mut cell = self.metas[*n as usize].clone();
                    cell.solution =
                        cell.solution.map(|t| t.replace(&mapping));
                    cell
                })
                .collect(),
            data: self.data.clone(),
            ..Self::default()
        };
        trial.skolem_kinds = declared
            .variables
            .iter()
            .copied()
            .zip(declared.kinds.iter().cloned())
            .collect();
        let actual = trial.instantiate(&compact);
        let expected = rigid(&declared.ty);
        let invalid = trial.unify(&actual, &expected, expr).is_err()
            || variables.iter().enumerate().any(|(i, n)| {
                self.metas[*n as usize].level <= self.level
                    && has_skolem(&trial.zonk(&Type::Var(i as u32)))
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
        env: &mut Scope,
        group: &BindGroup,
    ) -> Result<(), Diagnostic> {
        if group.binds.len() > 1 {
            let indices = group
                .binds
                .iter()
                .enumerate()
                .map(|(i, b)| (b.id.clone(), i))
                .collect::<BTreeMap<_, _>>();
            let edges = group
                .binds
                .iter()
                .map(|b| {
                    let mut refs = BTreeSet::new();
                    crate::renamed::references(&b.rhs, &mut refs);
                    refs.into_iter()
                        .filter_map(|id| indices.get(&id).copied())
                        .filter(|i| group.binds[*i].annotation.is_none())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            let components = crate::scc::components(&edges);
            if components.len() > 1 {
                for b in &group.binds {
                    if let Some(a) = &b.annotation {
                        env.push((
                            b.id.clone(),
                            self.data.signature(a, b.span)?,
                        ));
                    }
                }
                for component in components {
                    let recursive = component.len() > 1
                        || edges[component[0]].contains(&component[0]);
                    self.group(
                        env,
                        &BindGroup {
                            recursive,
                            binds: component
                                .into_iter()
                                .map(|i| group.binds[i].clone())
                                .collect(),
                        },
                    )?;
                }
                return Ok(());
            }
        }
        self.level += 1;
        let size = env.len();
        // Signatures enter the environment before inference, enabling polymorphic recursion.
        for b in &group.binds {
            if let Some(a) = &b.annotation {
                env.push((b.id.clone(), self.data.signature(a, b.span)?));
            }
        }
        let provisional = group
            .binds
            .iter()
            .map(|b| (b.id.clone(), self.fresh()))
            .collect::<Vec<_>>();
        if group.recursive {
            for (b, (id, t)) in group.binds.iter().zip(&provisional) {
                if b.annotation.is_none() {
                    env.push((id.clone(), Scheme::mono(t.clone())));
                }
            }
        }
        let mut inferred = vec![];
        let mut requirements = vec![];
        let mut annotations = vec![];
        for (b, (_, provisional)) in group.binds.iter().zip(&provisional) {
            let start = self.class_wanted.len();
            let (ty, givens) = if let Some(a) = &b.annotation {
                let declared = self.data.signature(a, b.span)?;
                let (expected, args) =
                    self.instantiate_with_arguments(&declared);
                let givens = Self::predicate_arguments(&declared, &args);
                self.infer_expected(env, &b.rhs, &expected).map_err(
                    |mut e| {
                        if e.code == ErrorCode::Mismatch {
                            e.code = ErrorCode::Signature;
                        }
                        e
                    },
                )?;
                (expected, givens)
            } else {
                (self.infer(env, &b.rhs)?, vec![])
            };
            if self.required_main.as_ref() == Some(&b.id) {
                let unit = self.data.tycon("()").ok_or_else(|| {
                    Diagnostic::error(
                        ErrorCode::Main,
                        "main requires IO () and the unit type",
                        b.span,
                    )
                })?;
                let expected = Type::Con(
                    self.data.tycon("IO").unwrap().id.clone(),
                    vec![Type::Con(unit.id.clone(), vec![])],
                );
                self.unify(&ty, &expected, &b.rhs).map_err(|mut e| {
                    e.code = ErrorCode::Main;
                    e.message = "main must have type IO ()".into();
                    e
                })?;
            }
            if group.recursive && b.annotation.is_none() {
                self.unify(provisional, &ty, &b.rhs)?;
            }
            inferred.push(ty);
            requirements.push(self.class_wanted.split_off(start));
            annotations.push(givens);
        }
        env.truncate(size);
        self.level -= 1;
        self.solve_fields(None)?;
        for (b, ty) in group.binds.iter().zip(&inferred) {
            if let Some(a) = &b.annotation {
                let inferred = self.generalize(ty.clone());
                let declared = self.data.signature(a, b.span)?;
                self.check_signature(&inferred, &declared, &b.rhs)?;
            }
        }
        self.solve_fields(Some(self.level))?;
        let mut residual = vec![];
        for ((b, wanteds), givens) in
            group.binds.iter().zip(requirements).zip(&mut annotations)
        {
            *givens =
                givens.iter().map(|p| self.zonk_predicate(p)).collect();
            for (wanted, span) in wanteds {
                let wanted = self.zonk_predicate(&wanted);
                let remaining =
                    self.classes.reduce(&wanted, givens, span)?;
                if b.annotation.is_some() {
                    for p in remaining {
                        if p.ty.variables().iter().any(|n| {
                            self.metas[*n as usize].level > self.level
                        }) {
                            return Err(Diagnostic::error(
                                ErrorCode::NoInstance,
                                format!(
                                    "signature does not entail {} {}",
                                    p.class, p.ty
                                ),
                                span,
                            ));
                        }
                        self.class_wanted.push((p, span));
                    }
                } else {
                    residual.extend(remaining);
                }
            }
        }
        let residual = self.classes.simplify(residual);
        for ((b, ty), givens) in
            group.binds.iter().zip(inferred).zip(annotations)
        {
            let mut scheme = self.generalize(ty);
            let free = scheme.ty.variables();
            let generalized =
                scheme.variables.iter().copied().collect::<BTreeSet<_>>();
            let ps = if b.annotation.is_some() {
                givens
            } else {
                residual.clone()
            };
            for p in ps {
                let vars = p.ty.variables();
                if !vars.is_subset(&free)
                    && vars
                        .iter()
                        .any(|n| self.metas[*n as usize].level > self.level)
                {
                    return Err(Diagnostic::error(
                        ErrorCode::Ambiguous,
                        format!(
                            "ambiguous constraint {} {}",
                            p.class, p.ty
                        ),
                        b.span,
                    ));
                }
                if vars.is_subset(&generalized) {
                    scheme.predicates.push(p);
                } else {
                    self.class_wanted.push((p, b.span));
                }
            }
            scheme.predicates = self.classes.simplify(scheme.predicates);
            self.schemes.insert(b.id.clone(), scheme.clone());
            env.push((b.id.clone(), scheme));
        }
        Ok(())
    }
    // ANCHOR_END: group
    fn pattern(
        &mut self,
        env: &mut Scope,
        pattern: &Pat,
        expected: &Type,
    ) -> Result<(), Diagnostic> {
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
            PatKind::Double(_) => {
                self.unify_at(expected, &Type::Double, pattern.span)?
            }
            PatKind::Int(_) => {
                self.unify_at(expected, &Type::Int, pattern.span)?
            }
            PatKind::Char(_) => {
                self.unify_at(expected, &Type::Char, pattern.span)?
            }
            PatKind::String(_) => {
                self.unify_at(expected, &Type::String, pattern.span)?
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
                self.unify_at(expected, &ty, pattern.span)?;
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
    /// Application trees do not introduce lexical scopes. Infer their children
    /// with an explicit postorder worklist, including long constructor spines.
    fn applications(
        &mut self,
        env: &mut Scope,
        root: &Expr,
    ) -> Result<Type, Diagnostic> {
        let mut work = vec![(root, false)];
        let mut types = Vec::new();
        while let Some((expr, ready)) = work.pop() {
            if let Kind::App(f, a) = &expr.kind {
                if !ready {
                    work.push((expr, true));
                    work.push((a, false));
                    work.push((f, false));
                } else {
                    let argument = types.pop().unwrap();
                    let function = types.pop().unwrap();
                    let result = self.fresh();
                    self.unify(
                        &function,
                        &Type::arrow(argument, result.clone()),
                        expr,
                    )?;
                    self.expression_types.insert(expr.node, result.clone());
                    types.push(result);
                }
            } else {
                types.push(self.infer(env, expr)?);
            }
        }
        Ok(types.pop().unwrap())
    }
    fn infer(
        &mut self,
        env: &mut Scope,
        expr: &Expr,
    ) -> Result<Type, Diagnostic> {
        let result = match &expr.kind {
            Kind::Annotated(e, syntax) => {
                let scheme = self.data.signature(syntax, expr.span)?;
                self.level += 1;
                let (expected, arguments) =
                    self.instantiate_with_arguments(&scheme);
                let givens = Self::predicate_arguments(&scheme, &arguments);
                let start = self.class_wanted.len();
                let actual = self.infer_expected(env, e, &expected);
                self.level -= 1;
                let actual = actual?;
                self.solve_fields(None)?;
                let inferred = self.generalize(actual.clone());
                self.check_signature(&inferred, &scheme, expr)?;
                let givens = givens
                    .iter()
                    .map(|p| self.zonk_predicate(p))
                    .collect::<Vec<_>>();
                let wanted = self.class_wanted.split_off(start);
                for (p, span) in wanted {
                    let p = self.zonk_predicate(&p);
                    for remaining in
                        self.classes.reduce(&p, &givens, span)?
                    {
                        if remaining.ty.variables().iter().any(|n| {
                            self.metas[*n as usize].level > self.level
                        }) {
                            return Err(Diagnostic::error(
                                ErrorCode::NoInstance,
                                "expression signature does not entail its constraints",
                                span,
                            ));
                        }
                        self.class_wanted.push((remaining, span));
                    }
                }
                self.class_wanted
                    .extend(givens.into_iter().map(|p| (p, expr.span)));
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
            Kind::Double(_) => Ok(Type::Double),
            Kind::Int(_) => Ok(Type::Int),
            Kind::Bool(_) => Ok(Type::Bool),
            Kind::Char(_) => Ok(Type::Char),
            Kind::String(_) => Ok(Type::String),
            Kind::Var(n) => {
                let scheme =
                    env.values.get(n).cloned().ok_or_else(|| {
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
                let predicates =
                    Self::predicate_arguments(&scheme, &arguments);
                self.class_wanted.extend(
                    predicates.iter().cloned().map(|p| (p, expr.span)),
                );
                self.occurrence_predicates.insert(expr.node, predicates);
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
            Kind::App(..) => self.applications(env, expr),
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
    pub evidence:
        BTreeMap<crate::evidence::EvVar, crate::classes::Evidence>,
    pub occurrence_predicates: BTreeMap<u32, Vec<Predicate>>,
    pub meta_kinds: Vec<crate::kinds::Kind>,
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
        required_main: sess
            .options
            .require_io
            .then(|| program.main.clone()),
        prelude_file: sess.prelude_file,
        classes: program.classes.clone(),
        data: std::rc::Rc::new(program.data.clone()),
        level: 1,
        metas: vec![],
        ..Infer::default()
    };
    let mut env = Scope::default();
    for (op, id) in &program.primitives {
        env.push((
            id.clone(),
            crate::primitives::signature(*op, &program.data),
        ));
    }
    for c in &program.classes.classes {
        for m in &c.methods {
            env.push((m.id.clone().unwrap(), m.scheme.clone()));
        }
    }
    // All annotated globals are visible through their declared schemes.
    for group in &program.groups {
        for b in &group.binds {
            if let Some(a) = &b.annotation {
                match infer.data.signature(a, b.span) {
                    Ok(s) => env.push((b.id.clone(), s)),
                    Err(e) => {
                        sess.emit(e);
                        return Err(Stop);
                    }
                }
            }
        }
    }
    let result = (|| {
        for group in &program.groups {
            infer.group(&mut env, group)?;
        }
        let main = infer.schemes[&program.main].clone();
        if sess.options.require_io {
            let expected = Type::Con(
                infer.data.tycon("IO").unwrap().id.clone(),
                vec![Type::Con(
                    infer.data.tycon("()").unwrap().id.clone(),
                    vec![],
                )],
            );
            if infer.zonk(&main.ty) != expected
                || !main.predicates.is_empty()
                || !main.variables.is_empty()
            {
                return Err(Diagnostic::error(
                    ErrorCode::Main,
                    "main must have type IO ()",
                    program
                        .groups
                        .iter()
                        .flat_map(|g| &g.binds)
                        .find(|b| b.id == program.main)
                        .unwrap()
                        .span,
                ));
            }
        }
        Ok(main.ty.clone())
    })();
    match result {
        Ok(ty) => {
            for (p, span) in std::mem::take(&mut infer.class_wanted) {
                let p = infer.zonk_predicate(&p);
                if let Err(mut e) = infer.classes.resolve(&p, &[], span) {
                    if !p.ty.variables().is_empty() {
                        e.code = ErrorCode::Ambiguous;
                    }
                    sess.emit(e);
                    return Err(Stop);
                }
            }
            let occurrence_predicates =
                std::mem::take(&mut infer.occurrence_predicates)
                    .into_iter()
                    .map(|(n, ps)| {
                        (
                            n,
                            ps.iter()
                                .map(|p| infer.zonk_predicate(p))
                                .collect(),
                        )
                    })
                    .collect();
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
                            predicates: s
                                .predicates
                                .iter()
                                .map(|p| infer.zonk_predicate(p))
                                .collect(),
                            variables: s.variables,
                            kinds: s.kinds,
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
            let mut typed = TypedProgram {
                evidence: BTreeMap::new(),
                occurrence_predicates,
                meta_kinds: infer
                    .metas
                    .iter()
                    .map(|m| m.kind.clone())
                    .collect(),
                resolved: infer.resolved,
                expression_types,
                surface: program,
                main_type,
                instantiations,
                parameters,
                schemes,
            };
            typed.evidence = match crate::evidence::record(&typed) {
                Ok(e) => e,
                Err(e) => {
                    sess.emit(e);
                    return Err(Stop);
                }
            };
            Ok(typed)
        }
        Err(error) => {
            sess.emit(error);
            Err(Stop)
        }
    }
}

impl TypedProgram {
    /// Present the principal qualified type with consistently renamed variables.
    pub fn main_description(&self) -> String {
        let scheme = &self.schemes[&self.surface.main];
        if scheme.predicates.is_empty() {
            return self.main_type.to_string();
        }
        fn names(t: &Type, map: &mut BTreeMap<u32, Type>) {
            match t {
                Type::Var(n) => {
                    let next = map.len() as u32;
                    map.entry(*n).or_insert(Type::Var(next));
                }
                Type::Con(_, args) => {
                    for a in args {
                        names(a, map);
                    }
                }
                Type::App(a, b) | Type::Arrow(a, b) => {
                    names(a, map);
                    names(b, map);
                }
                _ => {}
            }
        }
        let mut mapping = BTreeMap::new();
        names(&scheme.ty, &mut mapping);
        let ps = scheme
            .predicates
            .iter()
            .map(|p| format!("{} {}", p.class, p.ty.replace(&mapping)))
            .collect::<Vec<_>>();
        let context = if ps.len() == 1 {
            ps[0].clone()
        } else {
            format!("({})", ps.join(", "))
        };
        format!("{context} => {}", scheme.ty.replace(&mapping))
    }
}
