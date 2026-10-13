//! Level-based HM. Metavariables are mutable cells; zonking compresses their solution paths.
use crate::{
    ast::{self, Program},
    ast::{Expr, Kind, Op},
    diagnostics::{Diagnostic, ErrorCode},
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

#[derive(Default)]
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
    fn infer(
        &mut self,
        env: &mut Vec<(String, Scheme)>,
        expr: &Expr,
    ) -> Result<Type, Diagnostic> {
        match &expr.kind {
            Kind::Int(_) => Ok(Type::Int),
            Kind::Bool(_) => Ok(Type::Bool),
            Kind::Char(_) => Ok(Type::Char),
            Kind::String(_) => Ok(Type::String),
            Kind::Var(n) => {
                if n.starts_with("prim_") {
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
                            if n.starts_with(char::is_uppercase) {
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
                if n.starts_with("prim_") {
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
            Kind::Let(n, value, body) => {
                if n.starts_with("prim_") {
                    return Err(Diagnostic::error(
                        ErrorCode::PrivatePrimitive,
                        "primitive names are private to the Prelude",
                        expr.span,
                    ));
                }
                self.level += 1;
                let recursive = ast::free(value).contains(n);
                if recursive && !matches!(value.kind, Kind::Lam(..)) {
                    return Err(Diagnostic::error(
                        ErrorCode::RecursiveValue,
                        "recursive binding must be a function",
                        value.span,
                    ));
                }
                let provisional = self.fresh();
                if recursive {
                    env.push((
                        n.clone(),
                        Scheme::mono(provisional.clone()),
                    ));
                }
                let result = self.infer(env, value).and_then(|ty| {
                    if recursive {
                        self.unify(&provisional, &ty, value)?;
                    }
                    Ok(ty)
                });
                if recursive {
                    env.pop();
                }
                self.level -= 1;
                let scheme = self.generalize(result?);
                env.push((n.clone(), scheme));
                let result = self.infer(env, body);
                env.pop();
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
            Kind::Error => Err(Diagnostic::error(
                ErrorCode::Parse,
                "unrecovered syntax error",
                expr.span,
            )),
        }
    }
}

/// The checked surface tree and the inferred type of the selected entry point.
pub struct TypedProgram {
    pub surface: Program,
    pub main_type: Type,
}

/// Check ordered declarations using lexical scope maps and level-based HM.
pub fn check(
    sess: &mut Session,
    program: Program,
) -> Result<TypedProgram, Stop> {
    let mut names = std::collections::HashSet::new();
    for binding in &program.bindings {
        if !names.insert(&binding.name) {
            sess.emit(Diagnostic::error(
                ErrorCode::Duplicate,
                format!("duplicate definition {}", binding.name),
                binding.span,
            ));
        }
        if binding.name.starts_with("prim_") {
            sess.emit(Diagnostic::error(
                ErrorCode::PrivatePrimitive,
                "primitive names are private to the Prelude",
                binding.span,
            ));
        }
    }
    sess.finish(())?;
    let mut infer = Infer {
        level: 1,
        metas: vec![],
    };
    let mut env = if sess.options.no_prelude {
        vec![]
    } else {
        vec![
            (
                "div".into(),
                Scheme::mono(Type::arrow(
                    Type::Int,
                    Type::arrow(Type::Int, Type::Int),
                )),
            ),
            (
                "mod".into(),
                Scheme::mono(Type::arrow(
                    Type::Int,
                    Type::arrow(Type::Int, Type::Int),
                )),
            ),
        ]
    };
    match infer.infer(&mut env, &program.expression()) {
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
