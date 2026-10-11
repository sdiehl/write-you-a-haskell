// adapted from sdiehl/typechecker-zoo/type-classes/src/classes.rs (by_super, by_inst, resolve, context simplification)
//! Haskell 98 class environments. Matching never solves inference metavariables.
use crate::{
    ast::{self, TypeSyntax as T},
    data::DataEnv,
    diagnostics::{Diagnostic, ErrorCode},
    kinds::{Kind, Store},
    names::Id,
    source::Span,
    types::{Predicate, Scheme, Type},
};
use std::collections::{BTreeMap, BTreeSet};
#[derive(Clone, Debug)]
pub struct Method {
    pub name: String,
    pub syntax: T,
    pub scheme: Scheme,
    pub default: Option<ast::Binding>,
    pub id: Option<Id>,
}
#[derive(Clone, Debug)]
pub struct Class {
    pub name: String,
    pub parameter: String,
    pub kind: Kind,
    pub supers: Vec<String>,
    pub methods: Vec<Method>,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Instance {
    pub class: String,
    pub head: Type,
    pub variables: Vec<u32>,
    pub kinds: Vec<Kind>,
    pub context: Vec<Predicate>,
    pub methods: Vec<String>,
    pub span: Span,
}
#[derive(Clone, Debug, Default)]
pub struct Env {
    pub classes: Vec<Class>,
    pub instances: Vec<Instance>,
}
/// An immutable proof records superclass projections and instance application.
#[derive(Clone, Debug)]
// ANCHOR: evidence
pub enum Evidence {
    Given(usize, Vec<usize>),
    Instance(usize, Vec<Type>, Vec<Evidence>),
}
// ANCHOR_END: evidence
fn error(
    code: ErrorCode,
    message: impl Into<String>,
    span: Span,
) -> Diagnostic {
    Diagnostic::error(code, message, span)
}
fn replace(t: &T, map: &BTreeMap<String, T>) -> T {
    match t {
        T::Name(n) => map.get(n).cloned().unwrap_or_else(|| t.clone()),
        T::App(a, b) => {
            T::App(Box::new(replace(a, map)), Box::new(replace(b, map)))
        }
        T::Arrow(a, b) => {
            T::Arrow(Box::new(replace(a, map)), Box::new(replace(b, map)))
        }
        T::Qualified(ps, t) => {
            T::Qualified(ps.clone(), Box::new(replace(t, map)))
        }
    }
}
fn annotation(mut b: ast::Binding, ty: T) -> ast::Binding {
    b.rhs = ast::Expr {
        span: b.span,
        kind: ast::Kind::Annotated(Box::new(b.rhs), ty),
    };
    b
}
impl Env {
    pub fn class(&self, name: &str) -> &Class {
        self.classes
            .iter()
            .find(|c| c.name == name)
            .expect("validated class")
    }
    pub fn method(&self, id: &Id) -> Option<(&Class, &Method)> {
        self.classes.iter().find_map(|c| {
            c.methods
                .iter()
                .find(|m| m.id.as_ref() == Some(id))
                .map(|m| (c, m))
        })
    }
    fn super_path(&self, from: &str, to: &str) -> Option<Vec<usize>> {
        if from == to {
            return Some(vec![]);
        }
        for (i, s) in self.class(from).supers.iter().enumerate() {
            if let Some(mut path) = self.super_path(s, to) {
                path.insert(0, i);
                return Some(path);
            }
        }
        None
    }
    pub fn entails(&self, given: &Predicate, goal: &Predicate) -> bool {
        given.ty == goal.ty
            && self.super_path(&given.class, &goal.class).is_some()
    }
    pub fn simplify(&self, ps: Vec<Predicate>) -> Vec<Predicate> {
        let mut unique = vec![];
        for p in ps {
            if !unique.contains(&p) {
                unique.push(p);
            }
        }
        unique
            .iter()
            .enumerate()
            .filter(|(i, p)| {
                !unique
                    .iter()
                    .enumerate()
                    .any(|(j, q)| i != &j && self.entails(q, p))
            })
            .map(|(_, p)| p.clone())
            .collect()
    }
    pub fn by_instance(
        &self,
        goal: &Predicate,
    ) -> Option<(usize, Vec<Type>, Vec<Predicate>)> {
        self.instances.iter().enumerate().find_map(|(i, inst)| {
            if inst.class != goal.class {
                return None;
            }
            let mut map = BTreeMap::new();
            if !match_type(&inst.head, &goal.ty, &mut map) {
                return None;
            }
            Some((
                i,
                inst.variables.iter().map(|v| map[v].clone()).collect(),
                inst.context.iter().map(|p| p.replace(&map)).collect(),
            ))
        })
    }
    // ANCHOR: entailment
    pub fn resolve(
        &self,
        goal: &Predicate,
        givens: &[Predicate],
        span: Span,
    ) -> Result<Evidence, Diagnostic> {
        for (i, given) in givens.iter().enumerate() {
            if given.ty == goal.ty
                && let Some(path) =
                    self.super_path(&given.class, &goal.class)
            {
                return Ok(Evidence::Given(i, path));
            }
        }
        let (i, args, context) =
            self.by_instance(goal).ok_or_else(|| {
                error(
                    ErrorCode::NoInstance,
                    format!("no instance for {} {}", goal.class, goal.ty),
                    span,
                )
            })?;
        let proofs = context
            .iter()
            .map(|p| self.resolve(p, givens, span))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Evidence::Instance(i, args, proofs))
    }
    // ANCHOR_END: entailment
    pub fn reduce(
        &self,
        goal: &Predicate,
        givens: &[Predicate],
        span: Span,
    ) -> Result<Vec<Predicate>, Diagnostic> {
        if givens.iter().any(|p| self.entails(p, goal)) {
            return Ok(vec![]);
        }
        if matches!(goal.ty, Type::Var(_)) {
            return Ok(vec![goal.clone()]);
        }
        if let Type::App(head, _) = &goal.ty {
            let mut head = head.as_ref();
            while let Type::App(f, _) = head {
                head = f;
            }
            if matches!(head, Type::Var(_)) {
                let mut diagnostic = error(
                    ErrorCode::NoInstance,
                    format!(
                        "unsupported residual constraint: {} {}",
                        goal.class, goal.ty
                    ),
                    span,
                );
                diagnostic.notes.push("Fun generalizes class constraints only over a bare type variable; add a concrete type annotation".into());
                return Err(diagnostic);
            }
        }
        let (_, _, context) = self.by_instance(goal).ok_or_else(|| {
            error(
                ErrorCode::NoInstance,
                format!("no instance for {} {}", goal.class, goal.ty),
                span,
            )
        })?;
        let mut out = vec![];
        for p in context {
            out.extend(self.reduce(&p, givens, span)?);
        }
        Ok(out)
    }
}
/// One-way matching binds only variables from an instance declaration.
pub fn match_type(
    pattern: &Type,
    target: &Type,
    map: &mut BTreeMap<u32, Type>,
) -> bool {
    match (pattern, target) {
        (Type::Var(n), t) => {
            if let Some(old) = map.get(n) {
                old == t
            } else {
                map.insert(*n, t.clone());
                true
            }
        }
        (Type::Con(a, xs), Type::Con(b, ys)) => {
            a == b
                && xs.len() == ys.len()
                && xs.iter().zip(ys).all(|(x, y)| match_type(x, y, map))
        }
        (Type::Arrow(a, b), Type::Arrow(c, d))
        | (Type::App(a, b), Type::App(c, d)) => {
            match_type(a, c, map) && match_type(b, d, map)
        }
        _ => pattern == target,
    }
}
/// Check declarations and generate independently checked method implementations.
pub fn collect(
    program: &mut ast::Program,
    data: &mut DataEnv,
) -> Result<Env, Diagnostic> {
    let mut env = Env::default();
    let mut store = Store::default();
    let mut class_kinds = BTreeMap::new();
    for c in &program.classes {
        if data.kinds.contains_key(&c.name) {
            return Err(error(
                ErrorCode::Duplicate,
                "class name already names a type",
                c.span,
            ));
        }
        if class_kinds.insert(c.name.clone(), store.fresh()).is_some() {
            return Err(error(
                ErrorCode::Duplicate,
                "duplicate class",
                c.span,
            ));
        }
    }
    let mut method_names = BTreeSet::new();
    for c in &program.classes {
        let mut methods = vec![];
        let mut defaults = vec![];
        for d in &c.members {
            match d {
                ast::Declaration::Signature(names, ty, _) => {
                    if matches!(ty, T::Qualified(..)) {
                        return Err(error(
                            ErrorCode::Signature,
                            "class methods cannot add constraints",
                            c.span,
                        ));
                    }
                    for name in names {
                        if !method_names.insert(name.clone()) {
                            return Err(error(
                                ErrorCode::Duplicate,
                                format!("duplicate method {name}"),
                                c.span,
                            ));
                        }
                        methods.push(Method {
                            name: name.clone(),
                            syntax: ty.clone(),
                            scheme: Scheme::mono(Type::Int),
                            default: None,
                            id: None,
                        });
                    }
                }
                ast::Declaration::Binding(b) => {
                    defaults.push(ast::Declaration::Binding(b.clone()))
                }
                _ => {
                    return Err(error(
                        ErrorCode::Parse,
                        "a class body contains signatures and methods",
                        c.span,
                    ));
                }
            }
        }
        for b in crate::parse::declarations(defaults)? {
            let m = methods
                .iter_mut()
                .find(|m| m.name == b.name)
                .ok_or_else(|| {
                    error(
                        ErrorCode::Instance,
                        "default has no method signature",
                        b.span,
                    )
                })?;
            if m.default.is_some() {
                return Err(error(
                    ErrorCode::Duplicate,
                    "duplicate default method",
                    b.span,
                ));
            }
            m.default = Some(b);
        }
        for sup in &c.supers {
            if sup.variable != c.parameter {
                return Err(error(
                    ErrorCode::Signature,
                    "superclasses constrain the class parameter",
                    c.span,
                ));
            }
            if !class_kinds.contains_key(&sup.class) {
                return Err(error(
                    ErrorCode::UnboundConstructor,
                    "unknown superclass",
                    c.span,
                ));
            }
        }
        env.classes.push(Class {
            name: c.name.clone(),
            parameter: c.parameter.clone(),
            kind: Kind::Star,
            supers: c.supers.iter().map(|p| p.class.clone()).collect(),
            methods,
            span: c.span,
        });
    }
    // A DFS checks cycles before any entailment traverses superclass edges.
    fn visit(
        env: &Env,
        name: &str,
        active: &mut BTreeSet<String>,
        done: &mut BTreeSet<String>,
    ) -> Result<(), Diagnostic> {
        if done.contains(name) {
            return Ok(());
        }
        let c = env.class(name);
        if !active.insert(name.into()) {
            return Err(error(
                ErrorCode::Signature,
                "cyclic superclass hierarchy",
                c.span,
            ));
        }
        for s in &c.supers {
            visit(env, s, active, done)?;
        }
        active.remove(name);
        done.insert(name.into());
        Ok(())
    }
    let mut done = BTreeSet::new();
    for c in &env.classes {
        visit(&env, &c.name, &mut BTreeSet::new(), &mut done)?;
    }
    let indices = env
        .classes
        .iter()
        .enumerate()
        .map(|(i, c)| (c.name.clone(), i))
        .collect::<BTreeMap<_, _>>();
    let edges = env
        .classes
        .iter()
        .map(|c| c.supers.iter().map(|s| indices[s]).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    for component in crate::scc::components(&edges) {
        for i in component {
            let c = &mut env.classes[i];
            for sup in &c.supers {
                store.unify(
                    &class_kinds[&c.name],
                    &class_kinds[sup],
                    c.span,
                )?;
            }
            for m in &c.methods {
                let mut variables = BTreeMap::from([(
                    c.parameter.clone(),
                    class_kinds[&c.name].clone(),
                )]);
                let kind = store.infer(
                    &m.syntax,
                    &data.kinds,
                    &mut variables,
                    true,
                    c.span,
                )?;
                store.unify(&kind, &Kind::Star, c.span)?;
            }
            c.kind = store.finish(&class_kinds[&c.name]);
            class_kinds.insert(c.name.clone(), c.kind.clone());
            data.class_kinds.insert(c.name.clone(), c.kind.clone());
        }
    }
    for c in &mut env.classes {
        for m in &mut c.methods {
            let qualified = T::Qualified(
                vec![ast::PredicateSyntax {
                    class: c.name.clone(),
                    variable: c.parameter.clone(),
                }],
                Box::new(m.syntax.clone()),
            );
            m.scheme = data.signature(&qualified, c.span)?;
            let free = m.scheme.ty.variables();
            if m.scheme
                .predicates
                .iter()
                .any(|p| !p.ty.variables().is_subset(&free))
            {
                return Err(error(
                    ErrorCode::Ambiguous,
                    "ambiguous class method",
                    c.span,
                ));
            }
            if let Some(mut b) = m.default.clone() {
                b.name = format!("$dm_{}", m.name);
                program.bindings.push(annotation(b, qualified));
            }
        }
    }
    for (index, inst) in program.instances.iter().enumerate() {
        let class = env
            .classes
            .iter()
            .find(|c| c.name == inst.class)
            .ok_or_else(|| {
                error(
                    ErrorCode::UnboundConstructor,
                    "unknown instance class",
                    inst.span,
                )
            })?;
        let mut head = &inst.head;
        let mut arguments = vec![];
        while let T::App(f, a) = head {
            arguments.push(&**a);
            head = f;
        }
        if !matches!(head,T::Name(n) if n.starts_with(char::is_uppercase)||matches!(n.as_str(),"[]"|"()"|"(,)"|"(,,)"))
        {
            return Err(error(
                ErrorCode::Instance,
                "instance head must be nominal",
                inst.span,
            ));
        }
        if let T::Name(n) = head
            && data.aliases.iter().any(|a| a.name == *n)
        {
            return Err(error(
                ErrorCode::Instance,
                "an instance head cannot be a synonym",
                inst.span,
            ));
        }
        let mut vars = BTreeSet::new();
        for arg in arguments {
            if let T::Name(n) = arg
                && (n.starts_with(char::is_lowercase) || n.starts_with('_'))
                && vars.insert(n.clone())
            {
            } else {
                return Err(error(
                    ErrorCode::Instance,
                    "instance head arguments must be distinct type variables",
                    inst.span,
                ));
            }
        }
        if inst.context.iter().any(|p| !vars.contains(&p.variable)) {
            return Err(error(
                ErrorCode::Instance,
                "instance context mentions a variable outside its head",
                inst.span,
            ));
        }
        let scheme = data.signature_kind(
            &T::Qualified(
                inst.context.clone(),
                Box::new(inst.head.clone()),
            ),
            class.kind.clone(),
            inst.span,
        )?;
        if env.instances.iter().any(|old| {
            old.class == inst.class
                && match_type(&old.head, &scheme.ty, &mut BTreeMap::new())
        }) {
            return Err(error(
                ErrorCode::Instance,
                "overlapping instance",
                inst.span,
            ));
        }
        let mut methods = vec![];
        let mut supplied = BTreeSet::new();
        for b in &inst.methods {
            if !class.methods.iter().any(|m| m.name == b.name)
                || !supplied.insert(b.name.clone())
            {
                return Err(error(
                    ErrorCode::Instance,
                    "unknown or duplicate instance method",
                    b.span,
                ));
            }
            if matches!(b.rhs.kind, ast::Kind::Annotated(..)) {
                return Err(error(
                    ErrorCode::Signature,
                    "instance method signatures are not supported",
                    b.span,
                ));
            }
        }
        for m in &class.methods {
            let mut b = inst
                .methods
                .iter()
                .find(|b| b.name == m.name)
                .cloned()
                .or_else(|| m.default.clone())
                .ok_or_else(|| {
                    error(
                        ErrorCode::Instance,
                        format!("missing method {}", m.name),
                        inst.span,
                    )
                })?;
            b.name = format!("$i{index}_{}", m.name);
            methods.push(b.name.clone());
            fn names(t: &T, out: &mut BTreeSet<String>) {
                match t {
                    T::Name(n) => {
                        if n.starts_with(char::is_lowercase)
                            || n.starts_with('_')
                        {
                            out.insert(n.clone());
                        }
                    }
                    T::App(a, b) | T::Arrow(a, b) => {
                        names(a, out);
                        names(b, out);
                    }
                    T::Qualified(_, t) => names(t, out),
                }
            }
            let mut method_vars = BTreeSet::new();
            names(&m.syntax, &mut method_vars);
            let mut substitutions = method_vars
                .into_iter()
                .filter(|n| n != &class.parameter && vars.contains(n))
                .map(|n| {
                    let replacement =
                        T::Name(format!("_method{index}_{n}"));
                    (n, replacement)
                })
                .collect::<BTreeMap<_, _>>();
            substitutions
                .insert(class.parameter.clone(), inst.head.clone());
            let ty = replace(&m.syntax, &substitutions);
            program.bindings.push(annotation(
                b,
                T::Qualified(inst.context.clone(), Box::new(ty)),
            ));
        }
        env.instances.push(Instance {
            class: inst.class.clone(),
            head: scheme.ty,
            variables: scheme.variables,
            kinds: scheme.kinds,
            context: scheme.predicates,
            methods,
            span: inst.span,
        });
    }
    for inst in &env.instances {
        for sup in &env.class(&inst.class).supers {
            env.resolve(
                &Predicate {
                    class: sup.clone(),
                    ty: inst.head.clone(),
                },
                &inst.context,
                inst.span,
            )?;
        }
    }
    Ok(env)
}
