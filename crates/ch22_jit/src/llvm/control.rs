//! Lexical joins become blocks with phi parameters; each Flat terminal closes a block.
use super::*;
use crate::{
    core::{AltCon, Lit, Type},
    flat::{Atom, Body, Flat, Func, Rhs},
    runtime_layout as layout,
    source::Span,
};
use inkwell::IntPredicate;
/// Pending blocks live on a heap worklist, never on the Rust call stack.
struct Block<'ctx> {
    body: Body,
    block: BasicBlock<'ctx>,
}
impl<'ctx> Backend<'ctx, '_> {
    pub(super) fn function(&mut self, f: &Func) -> Result<()> {
        let fast = self.fast[f.name.0 as usize];
        self.entry(fast);
        let mut env = Env::new();
        if let Some(e) = &f.env {
            env.insert(
                e.clone(),
                fast.get_nth_param(0).unwrap().into_int_value(),
            );
        }
        for (i, p) in f.params.iter().enumerate() {
            env.insert(
                p.clone(),
                fast.get_nth_param(i as u32 + 1).unwrap().into_int_value(),
            );
        }
        self.body(&f.body, env, Joins::new())?;
        let slow = self.slow[f.name.0 as usize];
        self.entry(slow);
        let environment = slow.get_nth_param(0).unwrap().into_int_value();
        let array = slow.get_nth_param(1).unwrap().into_pointer_value();
        let mut args = vec![];
        for i in 0..f.params.len() {
            args.push(
                self.builder
                    .build_load(
                        self.word,
                        self.slot(array, i as u64)?,
                        "argument",
                    )?
                    .into_int_value(),
            );
        }
        // C-to-tailcc is an ABI adapter, not a guaranteed tail transfer.
        let value = self.known(f.name, Some(environment), &args, false)?;
        self.builder.build_return(Some(&value))?;
        Ok(())
    }
    fn rhs(&mut self, r: &Rhs, env: &Env<'ctx>) -> Result<IntValue<'ctx>> {
        Ok(match r {
            Rhs::Atom(a) => self.atom(a, env)?,
            Rhs::CallKnown(f, e, args) => {
                let e =
                    e.as_ref().map(|a| self.atom(a, env)).transpose()?;
                let args = self.atoms(args, env)?;
                self.known(*f, e, &args, false)?
            }
            Rhs::CallUnknown(f, args) => {
                let f = self.atom(f, env)?;
                let args = self.atoms(args, env)?;
                self.apply(f, &args)?
            }
            Rhs::AllocCon(c, args) => {
                let tag = self.program.metadata.constructors
                    [c.index as usize]
                    .tag;
                let args = self.atoms(args, env)?;
                if args.is_empty() {
                    self.n(layout::immediate(tag))
                } else {
                    self.allocation(layout::CON, tag, &args)?
                }
            }
            Rhs::AllocClosure(f, args) => {
                let args = self.atoms(args, env)?;
                self.closure(*f, &args)?
            }
            Rhs::EnvField(e, i) => self.load(
                env[e],
                *i as u64 + 1 + layout::CLOSURE_CAPTURES as u64,
            )?,
            Rhs::Field(a, i) => {
                let a = self.atom(a, env)?;
                self.load(a, *i as u64 + 1)?
            }
            Rhs::PatchClosure(a, i, v) => {
                let a = self.atom(a, env)?;
                let v = self.atom(v, env)?;
                self.void_call(
                    "rt_closure_set",
                    &[a.into(), self.n(*i as u64).into(), v.into()],
                )?;
                self.n(layout::UNIT)
            }
            Rhs::Prim(p, args, _) => {
                let args = self.atoms(args, env)?;
                self.primitive(*p, &args)?
            }
        })
    }
    pub(super) fn body(
        &mut self,
        b: &Body,
        mut env: Env<'ctx>,
        mut joins: Joins<'ctx>,
    ) -> Result<()> {
        // Unique binders index SSA values; lint already checked scopes.
        // Branches share this table without cloning lexical environments.
        let block = self.builder.get_insert_block().unwrap();
        let mut work = vec![Block {
            body: b.clone(),
            block,
        }];
        while let Some(Block { body, block }) = work.pop() {
            self.builder.position_at_end(block);
            let mut cursor = body.as_ref();
            while let Flat::Let(v, r, rest) = cursor {
                let value = self.rhs(r, &env)?;
                env.insert(v.clone(), value);
                cursor = rest;
            }
            self.terminal(cursor, &mut env, &mut joins, &mut work)?;
        }
        Ok(())
    }
    fn terminal(
        &mut self,
        node: &Flat,
        env: &mut Env<'ctx>,
        joins: &mut Joins<'ctx>,
        work: &mut Vec<Block<'ctx>>,
    ) -> Result<()> {
        match node {
            Flat::Let(..) => unreachable!(),
            Flat::Ret(a) => {
                let a = self.atom(a, env)?;
                self.builder.build_return(Some(&a))?;
            }
            Flat::TailCallKnown(f, e, args) => {
                let e =
                    e.as_ref().map(|a| self.atom(a, env)).transpose()?;
                let args = self.atoms(args, env)?;
                let value = self.known(*f, e, &args, true)?;
                self.builder.build_return(Some(&value))?;
            }
            Flat::TailCallUnknown(f, args) => {
                let f = self.atom(f, env)?;
                let args = self.atoms(args, env)?;
                let value = self.apply(f, &args)?;
                self.builder.build_return(Some(&value))?;
            }
            Flat::Join(id, params, b, rest) => {
                let predecessor = self.builder.get_insert_block().unwrap();
                let block = self.block("join");
                self.builder.position_at_end(block);
                let phis = params
                    .iter()
                    .map(|_| self.builder.build_phi(self.word, "parameter"))
                    .collect::<std::result::Result<Vec<_>, _>>()?;
                for (p, phi) in params.iter().zip(&phis) {
                    env.insert(
                        p.clone(),
                        phi.as_basic_value().into_int_value(),
                    );
                }
                work.push(Block {
                    body: b.clone(),
                    block,
                });
                joins.insert(
                    id.clone(),
                    Join {
                        block,
                        params: phis,
                    },
                );
                work.push(Block {
                    body: rest.clone(),
                    block: predecessor,
                });
            }
            Flat::Jump(id, args) => {
                let args = self.atoms(args, env)?;
                let from = self.builder.get_insert_block().unwrap();
                let join = &joins[id];
                for (phi, arg) in join.params.iter().zip(&args) {
                    phi.add_incoming(&[(arg, from)]);
                }
                self.builder.build_unconditional_branch(join.block)?;
            }
            Flat::Case(a, alts) => self.case(a, alts, env, work)?,
            Flat::Fail(span) => self.failure(*span)?,
        }
        Ok(())
    }
    fn tag(
        &self,
        value: IntValue<'ctx>,
        result: &Type,
    ) -> Result<IntValue<'ctx>> {
        let family = self
            .program
            .metadata
            .constructors
            .iter()
            .filter(|c| match (&c.result, result) {
                (Type::Con(a, _), Type::Con(b, _)) => a == b,
                (a, b) => a == b,
            })
            .collect::<Vec<_>>();
        let immediate = family.iter().any(|c| c.fields.is_empty());
        let pointer = family.iter().any(|c| !c.fields.is_empty());
        if !pointer {
            return Ok(self.builder.build_right_shift(
                value,
                self.n(1),
                false,
                "tag",
            )?);
        }
        if !immediate {
            return self.heap_tag(value);
        }
        let yes = self.block("immediate");
        let no = self.block("heap");
        let merge = self.block("tag");
        let bit = self.builder.build_and(value, self.n(1), "lowbit")?;
        let test = self.builder.build_int_compare(
            IntPredicate::NE,
            bit,
            self.n(0),
            "immediate",
        )?;
        self.builder.build_conditional_branch(test, yes, no)?;
        self.builder.position_at_end(yes);
        let a = self.builder.build_right_shift(
            value,
            self.n(1),
            false,
            "tag",
        )?;
        self.builder.build_unconditional_branch(merge)?;
        self.builder.position_at_end(no);
        let b = self.heap_tag(value)?;
        self.builder.build_unconditional_branch(merge)?;
        self.builder.position_at_end(merge);
        let phi = self.builder.build_phi(self.word, "tag")?;
        phi.add_incoming(&[(&a, yes), (&b, no)]);
        Ok(phi.as_basic_value().into_int_value())
    }
    fn heap_tag(&self, value: IntValue<'ctx>) -> Result<IntValue<'ctx>> {
        let h = self.load(value, 0)?;
        let shifted =
            self.builder.build_right_shift(h, self.n(8), false, "tag")?;
        Ok(self
            .builder
            .build_and(shifted, self.n(0x00ff_ffff), "tag")?)
    }
    fn case(
        &mut self,
        atom: &Atom,
        alts: &[(AltCon, Vec<Id>, Body)],
        env: &mut Env<'ctx>,
        work: &mut Vec<Block<'ctx>>,
    ) -> Result<()> {
        let value = self.atom(atom, env)?;
        let con = alts.iter().find_map(|(a, _, _)| {
            if let AltCon::Con(c) = a {
                Some(c)
            } else {
                None
            }
        });
        let discriminant = if let Some(c) = con {
            self.tag(
                value,
                &self.program.metadata.constructors[c.index as usize]
                    .result,
            )?
        } else {
            value
        };
        let fallback = self.block("default");
        let blocks = alts
            .iter()
            .map(|(a, _, _)| {
                if *a == AltCon::Default {
                    fallback
                } else {
                    self.block("alternative")
                }
            })
            .collect::<Vec<_>>();
        let mut cases = vec![];
        for ((alt, _, _), block) in alts.iter().zip(&blocks) {
            let key=match alt {
                AltCon::Con(c)=>self.program.metadata.constructors[c.index as usize].tag as u64,
                AltCon::Lit(Lit::Int(n))=>layout::integer(*n),
                AltCon::Lit(Lit::Char(c))=>layout::integer(*c as i64),
                AltCon::Default=>continue,
                _=>return Err(Error("internal error: string/Double pattern survived match compilation".into())),
            };
            cases.push((self.n(key), *block));
        }
        self.builder.build_switch(discriminant, fallback, &cases)?;
        for ((_, params, body), block) in alts.iter().zip(blocks) {
            self.builder.position_at_end(block);
            for (i, p) in params.iter().enumerate() {
                env.insert(p.clone(), self.load(value, i as u64 + 1)?);
            }
            work.push(Block {
                body: body.clone(),
                block,
            });
        }
        if !alts.iter().any(|(a, _, _)| *a == AltCon::Default) {
            self.builder.position_at_end(fallback);
            self.builder.build_unreachable()?;
        }
        Ok(())
    }
    fn failure(&self, span: Span) -> Result<()> {
        let site = self
            .program
            .metadata
            .locations
            .iter()
            .find(|(s, _)| *s == span)
            .map(|(_, s)| s.as_str())
            .unwrap_or("<unknown>:1:1");
        let mut parts = site.rsplitn(3, ':');
        let col = parts
            .next()
            .and_then(|s| s.parse::<u64>().ok())
            .ok_or_else(|| {
                super::Error("invalid pattern failure location".into())
            })?;
        let line = parts
            .next()
            .and_then(|s| s.parse::<u64>().ok())
            .ok_or_else(|| {
                super::Error("invalid pattern failure location".into())
            })?;
        let file = parts.next().ok_or_else(|| {
            super::Error("invalid pattern failure location".into())
        })?;
        let name = self
            .builder
            .build_global_string_ptr(file, "filename")?
            .as_pointer_value();
        let i32 = self.context.i32_type();
        self.void_call(
            "rt_pattern_fail",
            &[
                name.into(),
                i32.const_int(line, false).into(),
                i32.const_int(col, false).into(),
            ],
        )?;
        self.builder.build_unreachable()?;
        Ok(())
    }
    pub(super) fn startup(&mut self) -> Result<()> {
        let mut initializers = vec![];
        for g in &self.program.globals {
            if g.function.is_some() || self.inherited.contains(&g.name) {
                continue;
            }
            let f = self.module.add_function(
                &format!("fun.init{}", g.name.name.unique),
                self.word.fn_type(&[], false),
                Some(Linkage::Internal),
            );
            f.set_call_conventions(TAIL_CC);
            self.entry(f);
            self.body(&g.body, Env::new(), Joins::new())?;
            initializers.push((g.name.clone(), f));
        }
        let init = self.module.add_function(
            "fun_init",
            self.context.void_type().fn_type(&[], false),
            Some(Linkage::Internal),
        );
        self.entry(init);
        for g in &self.program.globals {
            if self.inherited.contains(&g.name) {
                continue;
            }
            if let Some(f) = g.function {
                let value = self.closure(f, &[])?;
                self.builder.build_store(
                    self.globals[&g.name].as_pointer_value(),
                    value,
                )?;
            }
        }
        for (id, f) in initializers {
            let call = self.builder.build_call(f, &[], "initializer")?;
            call.set_call_convention(TAIL_CC);
            self.builder.build_store(
                self.globals[&id].as_pointer_value(),
                call.try_as_basic_value().unwrap_basic(),
            )?;
        }
        self.builder.build_return(None)?;
        if self.jit {
            let entry = self.module.add_function(
                "fun_entry",
                self.context.void_type().fn_type(&[], false),
                None,
            );
            self.entry(entry);
            self.builder.build_call(init, &[], "")?;
            let action = self
                .builder
                .build_load(
                    self.word,
                    self.globals[&self.program.metadata.main.name]
                        .as_pointer_value(),
                    "main",
                )?
                .into_int_value();
            self.apply(action, &[self.n(layout::UNIT)])?;
            self.builder.build_return(None)?;
            return Ok(());
        }
        let main = self.module.add_function(
            "main",
            self.context.i32_type().fn_type(
                &[
                    self.context.i32_type().into(),
                    self.context.ptr_type(AddressSpace::default()).into(),
                ],
                false,
            ),
            None,
        );
        self.entry(main);
        self.void_call("rt_init", &[])?;
        self.builder.build_call(init, &[], "")?;
        let action = self
            .builder
            .build_load(
                self.word,
                self.globals[&self.program.metadata.main.name]
                    .as_pointer_value(),
                "main",
            )?
            .into_int_value();
        self.apply(action, &[self.n(layout::UNIT)])?;
        self.void_call(
            "rt_exit",
            &[self.context.i32_type().const_zero().into()],
        )?;
        self.builder.build_unreachable()?;
        Ok(())
    }
}
