//! Tagged words, heap slots and runtime calls. GEP uses explicit element types.
use super::*;
use crate::{
    core::Lit,
    flat::{Atom, FuncId},
    runtime_layout as layout,
};
use inkwell::{
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, PointerValue},
};

/// Storage is local to codegen. All ABI boundaries still carry tagged words.
#[derive(Clone, Copy)]
pub(super) enum Loc<'ctx> {
    Word(IntValue<'ctx>),
    Int(IntValue<'ctx>),
    Double(inkwell::values::FloatValue<'ctx>),
}

impl<'ctx> Backend<'ctx, '_> {
    pub(super) fn n(&self, n: u64) -> IntValue<'ctx> {
        self.word.const_int(n, false)
    }
    pub(super) fn external(
        &self,
        name: &str,
        args: &[BasicMetadataTypeEnum<'ctx>],
        result: bool,
    ) -> FunctionValue<'ctx> {
        self.module.get_function(name).unwrap_or_else(|| {
            let ty = if result {
                self.word.fn_type(args, false)
            } else {
                self.context.void_type().fn_type(args, false)
            };
            self.module.add_function(name, ty, None)
        })
    }
    pub(super) fn call(
        &self,
        name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Result<IntValue<'ctx>> {
        let types = args
            .iter()
            .map(|a| match a {
                BasicMetadataValueEnum::IntValue(v) => v.get_type().into(),
                BasicMetadataValueEnum::PointerValue(v) => {
                    v.get_type().into()
                }
                _ => unreachable!("runtime ABI operands"),
            })
            .collect::<Vec<_>>();
        let call = self.builder.build_call(
            self.external(name, &types, true),
            args,
            "call",
        )?;
        // A tailcc function must regain control to restore its callee-pop frame.
        // LLVM's tail-call pass may otherwise infer a tail transfer into the C ABI.
        call.set_tail_call_kind(
            inkwell::values::LLVMTailCallKind::LLVMTailCallKindNoTail,
        );
        Ok(call.try_as_basic_value().unwrap_basic().into_int_value())
    }
    pub(super) fn void_call(
        &self,
        name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> Result<()> {
        let types = args
            .iter()
            .map(|a| match a {
                BasicMetadataValueEnum::IntValue(v) => v.get_type().into(),
                BasicMetadataValueEnum::PointerValue(v) => {
                    v.get_type().into()
                }
                _ => unreachable!("runtime ABI operands"),
            })
            .collect::<Vec<_>>();
        self.builder.build_call(
            self.external(name, &types, false),
            args,
            "",
        )?;
        Ok(())
    }
    pub(super) fn pointer(
        &self,
        value: IntValue<'ctx>,
    ) -> Result<PointerValue<'ctx>> {
        Ok(self.builder.build_int_to_ptr(
            value,
            self.context.ptr_type(AddressSpace::default()),
            "ptr",
        )?)
    }
    pub(super) fn slot(
        &self,
        pointer: PointerValue<'ctx>,
        index: u64,
    ) -> Result<PointerValue<'ctx>> {
        // SAFETY: GEP indexes i64 words using the checked ABI layout.
        Ok(unsafe {
            self.builder.build_gep(
                self.word,
                pointer,
                &[self.n(index)],
                "slot",
            )?
        })
    }
    pub(super) fn load(
        &self,
        value: IntValue<'ctx>,
        index: u64,
    ) -> Result<IntValue<'ctx>> {
        Ok(self
            .builder
            .build_load(
                self.word,
                self.slot(self.pointer(value)?, index)?,
                "load",
            )?
            .into_int_value())
    }
    pub(super) fn store(
        &self,
        value: IntValue<'ctx>,
        index: u64,
        field: IntValue<'ctx>,
    ) -> Result<()> {
        self.builder
            .build_store(self.slot(self.pointer(value)?, index)?, field)?;
        Ok(())
    }
    pub(super) fn allocation(
        &self,
        kind: u8,
        tag: u32,
        fields: &[IntValue<'ctx>],
    ) -> Result<IntValue<'ctx>> {
        let name = if matches!(kind, layout::DOUBLE | layout::STRING) {
            "rt_alloc_atomic"
        } else {
            "rt_alloc"
        };
        let ptr = self.context.ptr_type(AddressSpace::default());
        let f = self.module.get_function(name).unwrap_or_else(|| {
            self.module.add_function(
                name,
                ptr.fn_type(&[self.word.into()], false),
                None,
            )
        });
        let size = (fields.len() + 1) as u64
            * if name == "rt_alloc" { 1 } else { 8 };
        let p = self
            .builder
            .build_call(f, &[self.n(size).into()], "alloc")?
            .try_as_basic_value()
            .unwrap_basic()
            .into_pointer_value();
        let value = self.builder.build_ptr_to_int(p, self.word, "word")?;
        self.store(
            value,
            0,
            self.n(layout::header(kind, tag, fields.len() as u32)),
        )?;
        for (i, field) in fields.iter().enumerate() {
            self.store(value, i as u64 + 1, *field)?;
        }
        Ok(value)
    }
    pub(super) fn closure(
        &self,
        f: FuncId,
        captures: &[IntValue<'ctx>],
    ) -> Result<IntValue<'ctx>> {
        let code = self.slow[f.0 as usize]
            .as_global_value()
            .as_pointer_value()
            .const_to_int(self.word);
        let mut fields = vec![
            code,
            self.n(self.program.funcs[f.0 as usize].params.len() as u64),
        ];
        fields.extend_from_slice(captures);
        self.allocation(layout::CLOSURE, 0, &fields)
    }
    fn string(&mut self, s: &str) -> IntValue<'ctx> {
        if let Some(v) = self.strings.get(s) {
            return *v;
        }
        let mut bytes = s.as_bytes().to_vec();
        bytes.resize(bytes.len().div_ceil(8) * 8, 0);
        let data = self.context.const_string(&bytes, false);
        let object = self.context.const_struct(
            &[
                self.n(layout::header(
                    layout::STRING,
                    0,
                    1 + (bytes.len() / 8) as u32,
                ))
                .into(),
                self.n(s.len() as u64).into(),
                data.into(),
            ],
            false,
        );
        let g = self.module.add_global(
            object.get_type(),
            None,
            &format!("fun.string{}", self.strings.len()),
        );
        g.set_linkage(Linkage::Private);
        g.set_constant(true);
        g.set_alignment(8);
        g.set_initializer(&object);
        let word = g.as_pointer_value().const_to_int(self.word);
        self.strings.insert(s.into(), word);
        word
    }
    /// Keep static string words in a retained data root table. Strings themselves are read-only.
    pub(super) fn string_roots(&self) {
        if self.strings.is_empty() {
            return;
        }
        let words = self.strings.values().copied().collect::<Vec<_>>();
        let array = self.word.const_array(&words);
        let roots = self.module.add_global(
            array.get_type(),
            None,
            "fun.string_roots",
        );
        roots.set_linkage(Linkage::Internal);
        roots.set_initializer(&array);
        roots.set_alignment(8);
        let ptr = self.context.ptr_type(AddressSpace::default());
        let used = ptr.const_array(&[roots.as_pointer_value()]);
        let keep =
            self.module.add_global(used.get_type(), None, "llvm.used");
        keep.set_linkage(Linkage::Appending);
        keep.set_section(Some("llvm.metadata"));
        keep.set_initializer(&used);
    }
    pub(super) fn literal(&mut self, l: &Lit) -> Result<IntValue<'ctx>> {
        Ok(match l {
            Lit::Int(n) => self.n(layout::integer(*n)),
            Lit::Char(c) => self.n(layout::integer(*c as i64)),
            Lit::String(s) => self.string(s),
            Lit::Double(bits) => {
                self.allocation(layout::DOUBLE, 0, &[self.n(*bits)])?
            }
        })
    }
    pub(super) fn atom(
        &mut self,
        a: &Atom,
        env: &Env<'ctx>,
    ) -> Result<Loc<'ctx>> {
        Ok(match a {
            Atom::Lit(Lit::Int(n)) => Loc::Int(self.n(*n as u64)),
            Atom::Lit(Lit::Double(bits)) => Loc::Double(
                self.context.f64_type().const_float(f64::from_bits(*bits)),
            ),
            Atom::Lit(l) => Loc::Word(self.literal(l)?),
            Atom::Var(v) => env[v],
            Atom::Global(v) => Loc::Word(
                self.builder
                    .build_load(
                        self.word,
                        self.globals[v].as_pointer_value(),
                        "global",
                    )?
                    .into_int_value(),
            ),
        })
    }
    pub(super) fn loc_word(
        &self,
        value: Loc<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        match value {
            Loc::Word(v) => Ok(v),
            Loc::Int(v) => self.retag(v),
            Loc::Double(v) => self.box_double(v),
        }
    }
    pub(super) fn loc_int(
        &self,
        value: Loc<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        match value {
            Loc::Word(v) => self.untag(v),
            Loc::Int(v) => Ok(v),
            Loc::Double(_) => {
                unreachable!("numeric evidence checked by Flat lint")
            }
        }
    }
    pub(super) fn loc_double(
        &self,
        value: Loc<'ctx>,
    ) -> Result<inkwell::values::FloatValue<'ctx>> {
        match value {
            Loc::Word(v) => self.unbox_double(v),
            Loc::Double(v) => Ok(v),
            Loc::Int(_) => {
                unreachable!("numeric evidence checked by Flat lint")
            }
        }
    }
    pub(super) fn atom_word(
        &mut self,
        a: &Atom,
        env: &Env<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        let loc = self.atom(a, env)?;
        self.loc_word(loc)
    }
    pub(super) fn atoms(
        &mut self,
        args: &[Atom],
        env: &Env<'ctx>,
    ) -> Result<Vec<IntValue<'ctx>>> {
        args.iter().map(|a| self.atom_word(a, env)).collect()
    }
    pub(super) fn known(
        &self,
        f: FuncId,
        env: Option<IntValue<'ctx>>,
        args: &[IntValue<'ctx>],
        tail: bool,
    ) -> Result<IntValue<'ctx>> {
        let mut actual: Vec<BasicMetadataValueEnum> =
            vec![env.unwrap_or(self.n(0)).into()];
        actual
            .extend(args.iter().map(|v| BasicMetadataValueEnum::from(*v)));
        let call = self.builder.build_call(
            self.fast[f.0 as usize],
            &actual,
            "known",
        )?;
        call.set_call_convention(TAIL_CC);
        call.set_tail_call(tail);
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_call_conventions()
            != TAIL_CC
        {
            call.set_tail_call_kind(
                inkwell::values::LLVMTailCallKind::LLVMTailCallKindNoTail,
            );
        }
        Ok(call.try_as_basic_value().unwrap_basic().into_int_value())
    }
    pub(super) fn apply(
        &self,
        f: IntValue<'ctx>,
        args: &[IntValue<'ctx>],
    ) -> Result<IntValue<'ctx>> {
        if (1..=4).contains(&args.len()) {
            let mut actual = vec![f.into()];
            actual.extend(
                args.iter().map(|v| BasicMetadataValueEnum::from(*v)),
            );
            return self.call(&format!("rt_apply{}", args.len()), &actual);
        }
        // Allocate in the entry, never in a loop reached by a join back-edge.
        let entry = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
            .unwrap();
        let alloc = self.context.create_builder();
        if let Some(first) = entry.get_first_instruction() {
            alloc.position_before(&first);
        } else {
            alloc.position_at_end(entry);
        }
        let array = alloc.build_alloca(
            self.word.array_type(args.len().max(1) as u32),
            "arguments",
        )?;
        for (i, a) in args.iter().enumerate() {
            self.builder.build_store(self.slot(array, i as u64)?, *a)?;
        }
        self.call(
            "rt_apply",
            &[f.into(), self.n(args.len() as u64).into(), array.into()],
        )
    }
    pub(super) fn box_double(
        &self,
        v: inkwell::values::FloatValue<'ctx>,
    ) -> Result<IntValue<'ctx>> {
        let bits = self
            .builder
            .build_bit_cast(v, self.word, "bits")?
            .into_int_value();
        self.allocation(layout::DOUBLE, 0, &[bits])
    }
    pub(super) fn unbox_double(
        &self,
        v: IntValue<'ctx>,
    ) -> Result<inkwell::values::FloatValue<'ctx>> {
        Ok(self
            .builder
            .build_bit_cast(
                self.load(v, 1)?,
                self.context.f64_type(),
                "double",
            )?
            .into_float_value())
    }
}
