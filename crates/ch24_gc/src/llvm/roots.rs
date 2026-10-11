//! Exact stack slots. A moving collection invalidates SSA words, never these addresses.
use super::*;
use crate::{
    flat::{Body, Flat},
    liveness,
};
use inkwell::values::PointerValue;
pub(super) struct Frame<'ctx> {
    array: PointerValue<'ctx>,
    slots: BTreeMap<Id, u64>,
    live: liveness::Liveness,
}
impl<'ctx> Backend<'ctx, '_> {
    pub(super) fn root_frame(
        &mut self,
        body: &Body,
        env: &mut Env<'ctx>,
    ) -> Result<()> {
        if !self.copying {
            return Ok(());
        }
        let mut live = liveness::analyze(body);
        live.binders.extend(env.keys().cloned());
        let slots = live
            .binders
            .iter()
            .enumerate()
            .map(|(i, v)| (v.clone(), i as u64))
            .collect::<BTreeMap<_, _>>();
        let count = slots.len() + live.scratch.max(2);
        let array = self
            .builder
            .build_alloca(self.word.array_type(count as u32), "roots")?;
        for i in 0..count {
            self.builder
                .build_store(self.slot(array, i as u64)?, self.n(0))?;
        }
        self.void_call(
            "rt_roots_push",
            &[array.into(), self.n(count as u64).into()],
        )?;
        self.frame = Some(Frame { array, slots, live });
        for (v, loc) in env.iter_mut() {
            *loc = self.root_binding(v, *loc)?;
        }
        Ok(())
    }
    // ANCHOR: root-binding
    pub(super) fn root_binding(
        &self,
        v: &Id,
        loc: Loc<'ctx>,
    ) -> Result<Loc<'ctx>> {
        let Some(frame) = &self.frame else {
            return Ok(loc);
        };
        if !matches!(loc, Loc::Word(_) | Loc::Root(_)) {
            return Ok(loc);
        }
        let p = self.slot(frame.array, frame.slots[v])?;
        let w = self.loc_word(loc)?;
        self.builder.build_store(p, w)?;
        Ok(Loc::Root(p))
    }
    // ANCHOR_END: root-binding
    pub(super) fn live_roots(&self, node: &Flat) -> Result<()> {
        let Some(frame) = &self.frame else {
            return Ok(());
        };
        let live = &frame.live.before[&liveness::key(node)];
        for (v, index) in &frame.slots {
            if !live.contains(v) {
                self.builder.build_store(
                    self.slot(frame.array, *index)?,
                    self.n(0),
                )?;
            }
        }
        for i in 0..frame.live.scratch.max(2) {
            self.builder.build_store(
                self.slot(
                    frame.array,
                    frame.slots.len() as u64 + i as u64,
                )?,
                self.n(0),
            )?;
        }
        Ok(())
    }
    pub(super) fn leave_roots(&self) -> Result<()> {
        if self.frame.is_some() {
            self.void_call("rt_roots_pop", &[])?;
        }
        Ok(())
    }
    /// Box each numeric argument into a scratch root before reloading all operands.
    pub(super) fn loc_words(
        &self,
        args: &[Loc<'ctx>],
    ) -> Result<Vec<IntValue<'ctx>>> {
        let Some(frame) = &self.frame else {
            return args.iter().map(|a| self.loc_word(*a)).collect();
        };
        let mut saved = vec![];
        for (i, a) in args.iter().enumerate() {
            let p = self
                .slot(frame.array, frame.slots.len() as u64 + i as u64)?;
            let w = self.loc_word(*a)?;
            self.builder.build_store(p, w)?;
            saved.push(Loc::Root(p));
        }
        saved.into_iter().map(|a| self.loc_word(a)).collect()
    }
}
