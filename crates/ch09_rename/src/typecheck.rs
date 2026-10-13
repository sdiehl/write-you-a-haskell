//! Level-based HM. Metavariables are mutable cells; zonking compresses their solution paths.
use crate::{
    ast::{self, Op},
    diagnostics::{Diagnostic, ErrorCode},
    names::Id,
    renamed::{BindGroup, Expr, Kind, Program},
    session::{Session, Stop},
    types::{self, Scheme, Type},
};
use std::collections::{BTreeMap, BTreeSet};

// ANCHOR: store
#[derive(Clone)]
struct Meta {
    level: u32,
    solution: Option<Type>,
}

#[derive(Clone, Default)]
struct Infer {
    level: u32,
    metas: Vec<Meta>,
}

impl Infer {
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
        fn replace(ty: &Type, mapping: &BTreeMap<u32, Type>) -> Type {
            match ty {
                Type::Var(n) => {
                    mapping.get(n).cloned().unwrap_or_else(|| ty.clone())
                }
                Type::Arrow(a, b) => {
                    Type::arrow(replace(a, mapping), replace(b, mapping))
                }
                other => other.clone(),
            }
        }
        let mapping = scheme
            .variables
            .iter()
            .map(|n| (*n, self.fresh()))
            .collect();
        replace(&scheme.ty, &mapping)
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
                Type::Arrow(a, b) => Type::arrow(rigid(a), rigid(b)),
                t => t.clone(),
            }
        }
        fn has_skolem(ty: &Type) -> bool {
            match ty {
                Type::Skolem(_) => true,
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
                let ty = self.infer(env, &b.rhs)?;
                if group.recursive {
                    self.unify(expected, &ty, &b.rhs)?;
                }
                Ok(ty)
            })
            .collect::<Result<Vec<_>, Diagnostic>>();
        env.truncate(size);
        self.level -= 1;
        let inferred = inferred?;
        for (binding, ty) in group.binds.iter().zip(&inferred) {
            if let Some(annotation) = &binding.annotation {
                let scheme = self.generalize(ty.clone());
                self.check_signature(&scheme, annotation, &binding.rhs)?;
            }
        }
        self.level += 1;
        for (binding, ty) in group.binds.iter().zip(&inferred) {
            if let Some(annotation) = &binding.annotation {
                let declared = self.instantiate(annotation);
                self.unify(ty, &declared, &binding.rhs)?;
            }
        }
        self.level -= 1;
        for (binding, ty) in group.binds.iter().zip(inferred) {
            env.push((binding.id.clone(), self.generalize(ty)));
        }
        Ok(())
    }
    // ANCHOR_END: group
    fn infer(
        &mut self,
        env: &mut Vec<(Id, Scheme)>,
        expr: &Expr,
    ) -> Result<Type, Diagnostic> {
        match &expr.kind {
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
                Ok(self.instantiate(&scheme))
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
        }
    }
}

/// The checked renamed tree and the inferred type of the selected entry point.
pub struct TypedProgram {
    pub surface: Program,
    pub main_type: Type,
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
    program: Program,
) -> Result<TypedProgram, Stop> {
    let mut infer = Infer {
        level: 1,
        metas: vec![],
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
                kind: Kind::Var(program.main.clone()),
                span: program.span,
            },
        )
    })();
    match result {
        Ok(ty) => Ok(TypedProgram {
            surface: program,
            main_type: infer.zonk(&ty).canonical(),
        }),
        Err(error) => {
            sess.emit(error);
            Err(Stop)
        }
    }
}
