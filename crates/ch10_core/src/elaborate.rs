//! HM generalization and instantiation become explicit System F abstractions and applications.
use crate::{
    ast::Op,
    core::{self, Bind, BindGroup, Expr, Id, Lit, Type},
    names, renamed,
    session::{Session, Stop},
    source::Span,
    typecheck::TypedProgram,
    types::{self, Scheme},
};
use std::collections::BTreeMap;

#[derive(Clone)]
struct Binder {
    id: Id,
    quantifiers: Vec<(u32, u32)>,
    global: bool,
}

struct Elaborator<'a> {
    typed: &'a TypedProgram,
    next_type: u32,
    binders: BTreeMap<names::Id, Binder>,
}

/// Unsolved inference variables absent from every enclosing scheme have no
/// observable type identity. Int is a closed witness for those internal holes.
fn convert(ty: &types::Type, variables: &BTreeMap<u32, u32>) -> Type {
    match ty {
        types::Type::Int => Type::Int,
        types::Type::Bool => Type::Bool,
        types::Type::Char => Type::Char,
        types::Type::String => Type::String,
        types::Type::Var(v) => {
            variables.get(v).map_or(Type::Int, |v| Type::Var(*v))
        }
        types::Type::Arrow(a, b) => {
            Type::arrow(convert(a, variables), convert(b, variables))
        }
        types::Type::Skolem(_) => {
            unreachable!("signature skolems cannot escape inference")
        }
    }
}

impl Elaborator<'_> {
    fn binder(
        &mut self,
        name: names::Id,
        scheme: &Scheme,
        outer: &BTreeMap<u32, u32>,
        global: bool,
    ) -> Binder {
        let mut variables = outer.clone();
        let mut quantifiers = Vec::new();
        for meta in &scheme.variables {
            let variable = self.next_type;
            self.next_type += 1;
            variables.insert(*meta, variable);
            quantifiers.push((*meta, variable));
        }
        let mut ty = convert(&scheme.ty, &variables);
        for (_, variable) in quantifiers.iter().rev() {
            ty = Type::Forall(*variable, Box::new(ty));
        }
        let binder = Binder {
            id: Id {
                name: name.clone(),
                ty,
            },
            quantifiers,
            global,
        };
        self.binders.insert(name, binder.clone());
        binder
    }
    // ANCHOR: generalize-core
    fn group(
        &mut self,
        group: &renamed::BindGroup,
        outer: &BTreeMap<u32, u32>,
        global: bool,
    ) -> BindGroup {
        for binding in &group.binds {
            self.binder(
                binding.id.clone(),
                &self.typed.schemes[&binding.id],
                outer,
                global,
            );
        }
        let binds = group
            .binds
            .iter()
            .map(|binding| {
                let binder = self.binders[&binding.id].clone();
                let mut variables = outer.clone();
                variables.extend(binder.quantifiers.iter().copied());
                let mut rhs = self.expression(&binding.rhs, &variables);
                for (_, variable) in binder.quantifiers.iter().rev() {
                    rhs = Expr::TyLam(*variable, Box::new(rhs));
                }
                Bind {
                    name: binder.id,
                    rhs: Box::new(rhs),
                }
            })
            .collect();
        BindGroup {
            recursive: group.recursive,
            binds,
        }
    }
    // ANCHOR_END: generalize-core
    fn expression(
        &mut self,
        expr: &renamed::Expr,
        variables: &BTreeMap<u32, u32>,
    ) -> Expr {
        use renamed::Kind;
        match &expr.kind {
            Kind::Int(n) => Expr::Lit(Lit::Int((n << 1) >> 1)),
            Kind::Bool(b) => Expr::Lit(Lit::Bool(*b)),
            Kind::Char(c) => Expr::Lit(Lit::Char(*c)),
            Kind::String(s) => Expr::Lit(Lit::String(s.clone())),
            Kind::Var(name) => {
                let binder = self.binders[name].clone();
                let mut result = if binder.global {
                    Expr::Global(binder.id)
                } else {
                    Expr::Var(binder.id)
                };
                let recorded = &self.typed.instantiations[&expr.node];
                // Recursive occurrences were inferred at a provisional monotype.
                // They now instantiate the component's generalized Core binder.
                let arguments = if recorded.is_empty()
                    && !binder.quantifiers.is_empty()
                {
                    binder
                        .quantifiers
                        .iter()
                        .map(|(meta, _)| {
                            convert(&types::Type::Var(*meta), variables)
                        })
                        .collect::<Vec<_>>()
                } else {
                    recorded
                        .iter()
                        .map(|ty| convert(ty, variables))
                        .collect()
                };
                assert_eq!(
                    arguments.len(),
                    binder.quantifiers.len(),
                    "elaboration instantiation arity"
                );
                for ty in arguments {
                    result = Expr::TyApp(Box::new(result), ty);
                }
                result
            }
            Kind::Lam(name, body) => {
                let ty = convert(&self.typed.parameters[name], variables);
                let id = Id {
                    name: name.clone(),
                    ty,
                };
                self.binders.insert(
                    name.clone(),
                    Binder {
                        id: id.clone(),
                        quantifiers: vec![],
                        global: false,
                    },
                );
                Expr::Lam(id, Box::new(self.expression(body, variables)))
            }
            Kind::App(a, b) => Expr::App(
                Box::new(self.expression(a, variables)),
                Box::new(self.expression(b, variables)),
            ),
            Kind::If(c, a, b) => Expr::If(
                Box::new(self.expression(c, variables)),
                Box::new(self.expression(a, variables)),
                Box::new(self.expression(b, variables)),
            ),
            Kind::Let(groups, body) => {
                let groups: Vec<_> = groups
                    .iter()
                    .map(|g| self.group(g, variables, false))
                    .collect();
                let mut body = self.expression(body, variables);
                for group in groups.into_iter().rev() {
                    body = Expr::Let(group, Box::new(body));
                }
                body
            }
            Kind::Binary(op, a, b) => Expr::Prim(
                *op,
                vec![],
                vec![
                    self.expression(a, variables),
                    self.expression(b, variables),
                ],
                expr.span,
            ),
            Kind::Neg(a) => Expr::Prim(
                Op::Sub,
                vec![],
                vec![Expr::Lit(Lit::Int(0)), self.expression(a, variables)],
                expr.span,
            ),
        }
    }
}

fn primitive(
    sess: &mut Session,
    name: names::Id,
    op: Op,
    span: Span,
) -> BindGroup {
    let operand = if op == Op::Append {
        Type::String
    } else {
        Type::Int
    };
    let result = if matches!(op, Op::Eq | Op::Lt | Op::Le) {
        Type::Bool
    } else {
        operand.clone()
    };
    let id = Id {
        name,
        ty: Type::arrow(
            operand.clone(),
            Type::arrow(operand.clone(), result),
        ),
    };
    let a = Id {
        name: sess.fresh_id("$a"),
        ty: operand.clone(),
    };
    let b = Id {
        name: sess.fresh_id("$b"),
        ty: operand,
    };
    let body = Expr::Prim(
        op,
        vec![],
        vec![Expr::Var(a.clone()), Expr::Var(b.clone())],
        span,
    );
    BindGroup {
        recursive: false,
        binds: vec![Bind {
            name: id,
            rhs: Box::new(Expr::Lam(
                a,
                Box::new(Expr::Lam(b, Box::new(body))),
            )),
        }],
    }
}

/// Elaborate zonked inference evidence into explicitly typed Core.
pub fn elaborate(
    sess: &mut Session,
    typed: TypedProgram,
) -> Result<core::Program, Stop> {
    let mut elaborator = Elaborator {
        typed: &typed,
        next_type: 0,
        binders: BTreeMap::new(),
    };
    let mut groups = Vec::new();
    for (op, name) in &typed.surface.primitives {
        let group = primitive(sess, name.clone(), *op, typed.surface.span);
        elaborator.binders.insert(
            name.clone(),
            Binder {
                id: group.binds[0].name.clone(),
                quantifiers: vec![],
                global: true,
            },
        );
        groups.push(group);
    }
    for group in &typed.surface.groups {
        groups.push(elaborator.group(group, &BTreeMap::new(), true));
    }
    let main = elaborator.binders[&typed.surface.main].id.clone();
    sess.finish(core::Program { groups, main })
}
