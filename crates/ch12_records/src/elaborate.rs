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
    sess: &'a mut Session,
    constructors: Vec<core::ConDecl>,
    locations: Vec<(Span, String)>,
    typed: &'a TypedProgram,
    next_type: u32,
    binders: BTreeMap<names::Id, Binder>,
}

/// Unsolved inference variables absent from every enclosing scheme have no
/// observable type identity. Int is a closed witness for those internal holes.
fn convert(ty: &types::Type, variables: &BTreeMap<u32, u32>) -> Type {
    match ty {
        types::Type::Con(id, args) => Type::Con(
            id.clone(),
            args.iter().map(|a| convert(a, variables)).collect(),
        ),
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
    fn pattern(
        &mut self,
        pattern: &renamed::Pat,
        variables: &BTreeMap<u32, u32>,
    ) -> core::Pat {
        use renamed::PatKind;
        match &pattern.kind {
            PatKind::Var(name) => {
                core::Pat::Var(self.pattern_binder(name, variables))
            }
            PatKind::As(name, p) => {
                let id = self.pattern_binder(name, variables);
                core::Pat::As(id, Box::new(self.pattern(p, variables)))
            }
            PatKind::Wild => core::Pat::Wild,
            PatKind::Con(id, ps) => core::Pat::Con(
                id.clone(),
                ps.iter().map(|p| self.pattern(p, variables)).collect(),
            ),
            PatKind::Int(n) => core::Pat::Lit(Lit::Int(*n)),
            PatKind::Char(c) => core::Pat::Lit(Lit::Char(*c)),
            PatKind::String(s) => core::Pat::Lit(Lit::String(s.clone())),
            PatKind::List(..) | PatKind::Tuple(..) => {
                unreachable!("pattern sugar must be lowered")
            }
        }
    }
    fn pattern_binder(
        &mut self,
        name: &names::Id,
        variables: &BTreeMap<u32, u32>,
    ) -> Id {
        let id = Id {
            name: name.clone(),
            ty: convert(&self.typed.parameters[name], variables),
        };
        self.binders.insert(
            name.clone(),
            Binder {
                id: id.clone(),
                quantifiers: vec![],
                global: false,
            },
        );
        id
    }
    fn row(
        &mut self,
        row: &renamed::Row,
        variables: &BTreeMap<u32, u32>,
    ) -> core::Row {
        let pats = row
            .patterns
            .iter()
            .map(|p| self.pattern(p, variables))
            .collect();
        let locals = row
            .locals
            .iter()
            .map(|g| self.group(g, variables, false))
            .collect();
        let body = match &row.body {
            renamed::RowBody::Plain(e) => core::RowBody::Plain(Box::new(
                self.expression(e, variables),
            )),
            renamed::RowBody::Guarded(gs) => core::RowBody::Guarded(
                gs.iter()
                    .map(|(g, e)| {
                        (
                            self.expression(g, variables),
                            self.expression(e, variables),
                        )
                    })
                    .collect(),
            ),
        };
        core::Row {
            pats,
            locals,
            body,
            span: row.span,
        }
    }
    /// Lower resolved records to ordinary constructor matches and strict lets.
    fn record_access(
        &mut self,
        expr: &renamed::Expr,
        receiver: &renamed::Expr,
        updates: Option<&[(String, renamed::Expr)]>,
        variables: &BTreeMap<u32, u32>,
    ) -> Expr {
        let (constructor, index) =
            self.typed.resolved[&(expr.node, 0)].clone();
        let declaration =
            self.constructors[constructor.index as usize].clone();
        let record_ty = convert(
            &self.typed.expression_types[&receiver.node],
            variables,
        );
        let Type::Con(_, arguments) = &record_ty else {
            unreachable!("resolved record type")
        };
        let scrut = Id {
            name: self.sess.fresh_id("$record"),
            ty: record_ty.clone(),
        };
        let fields = declaration
            .fields
            .iter()
            .map(|t| Id {
                name: self.sess.fresh_id("$field"),
                ty: instantiate(t, &declaration.parameters, arguments),
            })
            .collect::<Vec<_>>();
        let mut locals = vec![];
        let body = if let Some(updates) = updates {
            let mut values =
                fields.iter().cloned().map(Expr::Var).collect::<Vec<_>>();
            for (i, (_, value)) in updates.iter().enumerate() {
                let (_, slot) =
                    self.typed.resolved[&(expr.node, i)].clone();
                let id = Id {
                    name: self.sess.fresh_id("$update"),
                    ty: fields[slot].ty.clone(),
                };
                locals.push(BindGroup {
                    recursive: false,
                    binds: vec![Bind {
                        name: id.clone(),
                        rhs: Box::new(self.expression(value, variables)),
                    }],
                });
                values[slot] = Expr::Var(id);
            }
            Expr::Con(constructor.clone(), arguments.clone(), values)
        } else {
            Expr::Var(fields[index].clone())
        };
        let matched = Expr::Match(core::Match {
            scruts: vec![scrut.clone()],
            rows: vec![core::Row {
                pats: vec![core::Pat::Con(
                    constructor,
                    fields.into_iter().map(core::Pat::Var).collect(),
                )],
                locals,
                body: core::RowBody::Plain(Box::new(body)),
                span: expr.span,
            }],
            result: convert(
                &self.typed.expression_types[&expr.node],
                variables,
            ),
            span: expr.span,
        });
        Expr::Let(
            BindGroup {
                recursive: false,
                binds: vec![Bind {
                    name: scrut,
                    rhs: Box::new(self.expression(receiver, variables)),
                }],
            },
            Box::new(matched),
        )
    }
    fn expression(
        &mut self,
        expr: &renamed::Expr,
        variables: &BTreeMap<u32, u32>,
    ) -> Expr {
        use renamed::Kind;
        match &expr.kind {
            Kind::Annotated(e, _) => self.expression(e, variables),
            Kind::Project(e, _) => {
                self.record_access(expr, e, None, variables)
            }
            Kind::Update(e, fields) => {
                self.record_access(expr, e, Some(fields), variables)
            }
            // ANCHOR: record-construction
            Kind::Record(id, fields) => {
                let declaration =
                    self.constructors[id.index as usize].clone();
                let arguments = self.typed.instantiations[&expr.node]
                    .iter()
                    .map(|t| convert(t, variables))
                    .collect::<Vec<_>>();
                let mut values = vec![Expr::Lit(Lit::Int(0)); fields.len()];
                let mut locals = vec![];
                for (label, e) in fields {
                    let index = declaration
                        .labels
                        .as_ref()
                        .unwrap()
                        .iter()
                        .position(|n| n == label)
                        .unwrap();
                    let parameter = Id {
                        name: self.sess.fresh_id("$field"),
                        ty: instantiate(
                            &declaration.fields[index],
                            &declaration.parameters,
                            &arguments,
                        ),
                    };
                    locals.push(BindGroup {
                        recursive: false,
                        binds: vec![Bind {
                            name: parameter.clone(),
                            rhs: Box::new(self.expression(e, variables)),
                        }],
                    });
                    values[index] = Expr::Var(parameter);
                }
                let mut body = Expr::Con(id.clone(), arguments, values);
                for local in locals.into_iter().rev() {
                    body = Expr::Let(local, Box::new(body));
                }
                body
            }
            // ANCHOR_END: record-construction
            Kind::Int(n) => Expr::Lit(Lit::Int((n << 1) >> 1)),
            Kind::Bool(_) => unreachable!("Bool is an ordinary datatype"),
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
            Kind::If(..)
            | Kind::PatternLam(..)
            | Kind::Equations(..)
            | Kind::Case(..)
            | Kind::List(..)
            | Kind::Tuple(..) => {
                unreachable!("surface sugar must be lowered")
            }
            Kind::Con(id) => {
                let declaration =
                    self.constructors[id.index as usize].clone();
                let arguments = self.typed.instantiations[&expr.node]
                    .iter()
                    .map(|t| convert(t, variables))
                    .collect::<Vec<_>>();
                let fields = declaration
                    .fields
                    .iter()
                    .map(|t| {
                        instantiate(t, &declaration.parameters, &arguments)
                    })
                    .collect::<Vec<_>>();
                let parameters = fields
                    .into_iter()
                    .map(|ty| Id {
                        name: self.sess.fresh_id("$field"),
                        ty,
                    })
                    .collect::<Vec<_>>();
                let mut body = Expr::Con(
                    id.clone(),
                    arguments,
                    parameters.iter().cloned().map(Expr::Var).collect(),
                );
                for parameter in parameters.into_iter().rev() {
                    body = Expr::Lam(parameter, Box::new(body));
                }
                body
            }
            Kind::Match(scruts, rows) => {
                let result = convert(
                    &self.typed.expression_types[&expr.node],
                    variables,
                );
                let mut bindings = Vec::new();
                let mut ids = Vec::new();
                for scrut in scruts {
                    let id = Id {
                        name: self.sess.fresh_id("$scrut"),
                        ty: convert(
                            &self.typed.expression_types[&scrut.node],
                            variables,
                        ),
                    };
                    bindings.push(BindGroup {
                        recursive: false,
                        binds: vec![Bind {
                            name: id.clone(),
                            rhs: Box::new(
                                self.expression(scrut, variables),
                            ),
                        }],
                    });
                    ids.push(id);
                }
                let rows = rows
                    .iter()
                    .map(|row| self.row(row, variables))
                    .collect();
                let (line, column) = self.sess.sources.line_col(expr.span);
                self.locations.push((
                    expr.span,
                    format!(
                        "{}:{line}:{column}",
                        self.sess.sources.get(expr.span.file).name
                    ),
                ));
                let mut body = Expr::Match(core::Match {
                    scruts: ids,
                    rows,
                    result,
                    span: expr.span,
                });
                for binding in bindings.into_iter().rev() {
                    body = Expr::Let(binding, Box::new(body));
                }
                body
            }
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
    let mut constructors = Vec::new();
    let mut next_type = 0;
    for info in &typed.surface.data.constructors {
        let mut variables = BTreeMap::new();
        let mut parameters = Vec::new();
        for meta in &info.scheme.variables {
            variables.insert(*meta, next_type);
            parameters.push(next_type);
            next_type += 1;
        }
        let mut result = &info.scheme.ty;
        for _ in &info.fields {
            let types::Type::Arrow(_, b) = result else {
                unreachable!()
            };
            result = b;
        }
        constructors.push(core::ConDecl {
            labels: info.labels.clone(),
            id: info.id.clone(),
            tag: info.tag,
            parameters,
            fields: info
                .fields
                .iter()
                .map(|t| convert(t, &variables))
                .collect(),
            result: convert(result, &variables),
        });
    }
    let mut groups = Vec::new();
    let mut binders = BTreeMap::new();
    for (op, name) in &typed.surface.primitives {
        let group = primitive(sess, name.clone(), *op, typed.surface.span);
        binders.insert(
            name.clone(),
            Binder {
                id: group.binds[0].name.clone(),
                quantifiers: vec![],
                global: true,
            },
        );
        groups.push(group);
    }
    let mut elaborator = Elaborator {
        sess,
        typed: &typed,
        next_type,
        binders,
        constructors,
        locations: vec![],
    };
    for group in &typed.surface.groups {
        groups.push(elaborator.group(group, &BTreeMap::new(), true));
    }
    let main = elaborator.binders[&typed.surface.main].id.clone();
    let program = core::Program {
        groups,
        main,
        constructors: elaborator.constructors,
        locations: elaborator.locations,
        data_types: typed
            .surface
            .data
            .types
            .iter()
            .map(|t| (t.id.clone(), t.arity))
            .collect(),
    };
    elaborator.sess.finish(program)
}

/// Instantiate immutable constructor fields; the checker uses the same ABI type order.
pub fn instantiate(
    ty: &Type,
    variables: &[u32],
    arguments: &[Type],
) -> Type {
    let mut ty = ty.clone();
    for (v, a) in variables.iter().zip(arguments) {
        ty = ty.substitute(*v, a);
    }
    ty
}
