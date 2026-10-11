//! Class evidence becomes ordinary typed constructors, lambdas, and projections.
use super::*;
use crate::{classes::Evidence, types::Predicate};
/// Declare dictionary types before elaborating any method or instance.
pub(super) fn declare(
    typed: &TypedProgram,
    next: &mut u32,
    kinds: &mut BTreeMap<u32, crate::kinds::Kind>,
    constructors: &mut Vec<core::ConDecl>,
    types: &mut Vec<(crate::data::TyConId, crate::kinds::Kind)>,
) -> BTreeMap<String, usize> {
    let mut heads = BTreeMap::new();
    for c in &typed.surface.classes.classes {
        let id = crate::data::TyConId {
            index: types.len() as u32,
            name: format!("$D{}", c.name),
        };
        types.push((
            id.clone(),
            crate::kinds::Kind::arrow(
                c.kind.clone(),
                crate::kinds::Kind::Star,
            ),
        ));
        heads.insert(c.name.clone(), id);
    }
    let mut dicts = BTreeMap::new();
    for c in &typed.surface.classes.classes {
        let parameter = *next;
        *next += 1;
        kinds.insert(parameter, c.kind.clone());
        let mut fields = c
            .supers
            .iter()
            .map(|s| {
                Type::Con(heads[s].clone(), vec![Type::Var(parameter)])
            })
            .collect::<Vec<_>>();
        for m in &c.methods {
            let types::Type::Var(class_var) = m.scheme.predicates[0].ty
            else {
                unreachable!()
            };
            let mut variables = BTreeMap::from([(class_var, parameter)]);
            let mut extra = vec![];
            for (v, k) in m.scheme.variables.iter().zip(&m.scheme.kinds) {
                if *v != class_var {
                    variables.insert(*v, *next);
                    kinds.insert(*next, k.clone());
                    extra.push(*next);
                    *next += 1;
                }
            }
            let mut ty = convert(&m.scheme.ty, &variables);
            for v in extra.into_iter().rev() {
                ty = Type::Forall(v, Box::new(ty));
            }
            fields.push(ty);
        }
        let index = constructors.len();
        dicts.insert(c.name.clone(), index);
        constructors.push(core::ConDecl {
            id: crate::data::ConId {
                index: index as u32,
                name: format!("$MkD{}", c.name),
            },
            tag: 0,
            parameters: vec![parameter],
            fields,
            result: Type::Con(
                heads[&c.name].clone(),
                vec![Type::Var(parameter)],
            ),
            labels: None,
        });
    }
    dicts
}
impl Elaborator<'_> {
    // ANCHOR: dictionary-type
    pub(super) fn dictionary_type(&self, class: &str, ty: Type) -> Type {
        let c = &self.constructors[self.dicts[class]];
        instantiate(&c.result, &c.parameters, &[ty])
    }
    // ANCHOR_END: dictionary-type
    fn projection(
        &mut self,
        class: &str,
        ty: Type,
        dict: Expr,
        index: usize,
    ) -> Expr {
        let c = self.constructors[self.dicts[class]].clone();
        let span = self.typed.surface.span;
        let id = Id {
            name: self.sess.fresh_id("$dictionary"),
            ty: self.dictionary_type(class, ty.clone()),
        };
        let fields = c
            .fields
            .iter()
            .enumerate()
            .map(|(n, t)| Id {
                name: self.sess.fresh_id(&format!("$slot{n}")),
                ty: instantiate(
                    t,
                    &c.parameters,
                    std::slice::from_ref(&ty),
                ),
            })
            .collect::<Vec<_>>();
        let result = fields[index].ty.clone();
        let selected = Expr::Var(fields[index].clone());
        Expr::Let(
            BindGroup {
                recursive: false,
                binds: vec![Bind {
                    name: id.clone(),
                    rhs: Box::new(dict),
                }],
            },
            Box::new(Expr::Match(core::Match {
                scruts: vec![id],
                result,
                span,
                rows: vec![core::Row {
                    pats: vec![core::Pat::Con(
                        c.id,
                        fields.into_iter().map(core::Pat::Var).collect(),
                    )],
                    locals: vec![],
                    body: core::RowBody::Plain(Box::new(selected)),
                    span,
                }],
            })),
        )
    }
    pub(super) fn evidence(
        &mut self,
        p: &Predicate,
        variables: &BTreeMap<u32, u32>,
    ) -> Expr {
        let givens = self
            .givens
            .iter()
            .map(|(p, _)| p.clone())
            .collect::<Vec<_>>();
        let proof = self
            .typed
            .surface
            .classes
            .resolve(p, &givens, self.typed.surface.span)
            .expect("type inference proved every constraint");
        self.proof(&proof, variables)
    }
    pub(super) fn proof(
        &mut self,
        proof: &Evidence,
        variables: &BTreeMap<u32, u32>,
    ) -> Expr {
        match proof {
            Evidence::Given(i, path) => {
                let (p, id) = self.givens[*i].clone();
                let mut class = p.class;
                let ty = self.convert(&p.ty, variables);
                let mut expr = Expr::Var(id);
                for &index in path {
                    expr = self.projection(&class, ty.clone(), expr, index);
                    class = self.typed.surface.classes.class(&class).supers
                        [index]
                        .clone();
                }
                expr
            }
            Evidence::Instance(i, args, context) => {
                let mut expr =
                    Expr::Global(self.instance_ids[*i].0.clone());
                for t in args {
                    expr = Expr::TyApp(
                        Box::new(expr),
                        self.convert(t, variables),
                    );
                }
                for proof in context {
                    let argument = self.proof(proof, variables);
                    expr = Expr::App(Box::new(expr), Box::new(argument));
                }
                expr
            }
        }
    }
    pub(super) fn selector(
        &mut self,
        class: &crate::classes::Class,
        index: usize,
    ) -> BindGroup {
        let m = &class.methods[index];
        let binder = self.binders[m.id.as_ref().unwrap()].clone();
        let variables = binder
            .quantifiers
            .iter()
            .copied()
            .collect::<BTreeMap<_, _>>();
        let p = &m.scheme.predicates[0];
        let ty = self.convert(&p.ty, &variables);
        let dict = Id {
            name: self.sess.fresh_id("$given"),
            ty: self.dictionary_type(&class.name, ty.clone()),
        };
        let mut body = self.projection(
            &class.name,
            ty,
            Expr::Var(dict.clone()),
            class.supers.len() + index,
        );
        let types::Type::Var(class_var) = p.ty else {
            unreachable!()
        };
        for v in &m.scheme.variables {
            if *v != class_var {
                body = Expr::TyApp(Box::new(body), Type::Var(variables[v]));
            }
        }
        body = Expr::Lam(dict, Box::new(body));
        for (_, v) in binder.quantifiers.iter().rev() {
            body = Expr::TyLam(*v, Box::new(body));
        }
        BindGroup {
            recursive: false,
            binds: vec![Bind {
                name: binder.id,
                rhs: Box::new(body),
            }],
        }
    }
    pub(super) fn declare_instances(&mut self) {
        for inst in &self.typed.surface.classes.instances {
            let mut variables = BTreeMap::new();
            let mut quantifiers = vec![];
            for (v, k) in inst.variables.iter().zip(&inst.kinds) {
                let n = self.next_type;
                self.next_type += 1;
                self.type_kinds.insert(n, k.clone());
                variables.insert(*v, n);
                quantifiers.push(n);
            }
            let result = self.dictionary_type(
                &inst.class,
                self.convert(&inst.head, &variables),
            );
            let mut ty = result;
            for p in inst.context.iter().rev() {
                ty = Type::arrow(
                    self.dictionary_type(
                        &p.class,
                        self.convert(&p.ty, &variables),
                    ),
                    ty,
                );
            }
            for v in quantifiers.iter().rev() {
                ty = Type::Forall(*v, Box::new(ty));
            }
            let id = Id {
                name: self.sess.fresh_id(&format!(
                    "$f{}_{}",
                    inst.class,
                    self.instance_ids.len()
                )),
                ty,
            };
            self.instance_ids.push((id, variables));
        }
    }
    pub(super) fn instance(&mut self, index: usize) -> BindGroup {
        let inst = self.typed.surface.classes.instances[index].clone();
        let class = self.typed.surface.classes.class(&inst.class).clone();
        let (id, mut variables) = self.instance_ids[index].clone();
        let saved = self.givens.clone();
        let mut dicts = vec![];
        for p in &inst.context {
            let id = Id {
                name: self.sess.fresh_id("$context"),
                ty: self.dictionary_type(
                    &p.class,
                    self.convert(&p.ty, &variables),
                ),
            };
            self.givens.push((p.clone(), id.clone()));
            dicts.push(id);
        }
        let mut fields = vec![];
        for sup in &class.supers {
            fields.push(self.evidence(
                &Predicate {
                    class: sup.clone(),
                    ty: inst.head.clone(),
                },
                &variables,
            ));
        }
        for (m, name) in class.methods.iter().zip(&inst.methods) {
            let types::Type::Var(class_var) = m.scheme.predicates[0].ty
            else {
                unreachable!()
            };
            let mut substitutions =
                BTreeMap::from([(class_var, inst.head.clone())]);
            let mut extra = vec![];
            for (v, k) in m.scheme.variables.iter().zip(&m.scheme.kinds) {
                if *v != class_var {
                    let n = self.next_type;
                    self.next_type += 1;
                    self.type_kinds.insert(n, k.clone());
                    // These are local type variables, not inference-store indices.
                    let temporary = variables
                        .keys()
                        .chain(m.scheme.variables.iter())
                        .copied()
                        .max()
                        .map_or(0, |n| {
                            n.checked_add(1).expect("type variable supply")
                        });
                    variables.insert(temporary, n);
                    substitutions.insert(*v, types::Type::Var(temporary));
                    extra.push(n);
                }
            }
            let expected = m.scheme.ty.replace(&substitutions);
            let (name, scheme) = self
                .typed
                .schemes
                .iter()
                .find(|(id, _)| id.text == *name)
                .expect("instance implementation");
            let binder = self.binders[name].clone();
            let mut mapping = BTreeMap::new();
            assert!(crate::classes::match_type(
                &scheme.ty,
                &expected,
                &mut mapping
            ));
            let mut body = Expr::Global(binder.id);
            for (meta, _) in &binder.quantifiers {
                body = Expr::TyApp(
                    Box::new(body),
                    self.convert(&mapping[meta], &variables),
                );
            }
            for p in &scheme.predicates {
                let proof = self.evidence(&p.replace(&mapping), &variables);
                body = Expr::App(Box::new(body), Box::new(proof));
            }
            for v in extra.into_iter().rev() {
                body = Expr::TyLam(v, Box::new(body));
            }
            fields.push(body);
        }
        let con = self.constructors[self.dicts[&inst.class]].id.clone();
        let mut body = Expr::Con(
            con,
            vec![self.convert(&inst.head, &variables)],
            fields,
        );

        for dict in dicts.into_iter().rev() {
            body = Expr::Lam(dict, Box::new(body));
        }
        for v in inst.variables.iter().rev() {
            body = Expr::TyLam(variables[v], Box::new(body));
        }
        self.givens = saved;
        BindGroup {
            recursive: false,
            binds: vec![Bind {
                name: id,
                rhs: Box::new(body),
            }],
        }
    }
}
