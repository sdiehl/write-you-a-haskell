// adapted from sdiehl/prism/crates/prism-native/src/codegen/llvm.rs (target and pass setup)
//! Flat IR to verified, host-targeted LLVM. Source values uniformly use i64.
mod control;
mod primitives;
mod values;
use crate::{flat, names::Id};
use inkwell::{
    AddressSpace, OptimizationLevel,
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target,
        TargetMachine,
    },
    types::IntType,
    values::{FunctionValue, GlobalValue, IntValue, PhiValue},
};
use std::{collections::BTreeMap, path::Path, sync::OnceLock};
/// LLVM's Tail calling convention (llvm/IR/CallingConv.h).
const TAIL_CC: u32 = 18;
type Result<T> = std::result::Result<T, Error>;
/// Builder, verifier, target and object-emission failures share one driver boundary.
#[derive(Debug)]
pub struct Error(pub String);
impl From<BuilderError> for Error {
    fn from(e: BuilderError) -> Self {
        Self(e.to_string())
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
impl std::error::Error for Error {}
#[derive(Clone)]
struct Join<'ctx> {
    block: BasicBlock<'ctx>,
    params: Vec<PhiValue<'ctx>>,
}
type Env<'ctx> = BTreeMap<Id, IntValue<'ctx>>;
type Joins<'ctx> = BTreeMap<Id, Join<'ctx>>;
/// All LLVM values borrow a caller-owned Context; no self-referential ownership.
pub struct Backend<'ctx, 'program> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    machine: TargetMachine,
    program: &'program flat::Program,
    word: IntType<'ctx>,
    fast: Vec<FunctionValue<'ctx>>,
    slow: Vec<FunctionValue<'ctx>>,
    globals: BTreeMap<Id, GlobalValue<'ctx>>,
    strings: BTreeMap<String, IntValue<'ctx>>,
}
impl<'ctx, 'program> Backend<'ctx, 'program> {
    /// Declare every entry first, then emit bodies and startup. Always verify.
    pub fn new(
        context: &'ctx Context,
        program: &'program flat::Program,
        opt: u8,
    ) -> Result<Self> {
        crate::flat_lint::lint(program).map_err(Error)?;
        static INIT: OnceLock<std::result::Result<(), String>> =
            OnceLock::new();
        INIT.get_or_init(|| {
            Target::initialize_native(&InitializationConfig::default())
        })
        .clone()
        .map_err(Error)?;
        let level = match opt {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            3 => OptimizationLevel::Aggressive,
            _ => {
                return Err(Error(
                    "optimization level must be 0..3".into(),
                ));
            }
        };
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| Error(e.to_string()))?;
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                level,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or_else(|| {
                Error("cannot create LLVM target machine".into())
            })?;
        if machine.get_target_data().get_pointer_byte_size(None) != 8 {
            return Err(Error("Fun requires a 64-bit target".into()));
        }
        let module = context.create_module("Fun");
        module.set_triple(&triple);
        module
            .set_data_layout(&machine.get_target_data().get_data_layout());
        let mut this = Self {
            context,
            module,
            builder: context.create_builder(),
            machine,
            program,
            word: context.i64_type(),
            fast: vec![],
            slow: vec![],
            globals: BTreeMap::new(),
            strings: BTreeMap::new(),
        };
        this.declare();
        for f in &program.funcs {
            this.function(f)?;
        }
        this.startup()?;
        this.string_roots();
        this.verify()?;
        Ok(this)
    }
    fn declare(&mut self) {
        let ptr = self.context.ptr_type(AddressSpace::default());
        for f in &self.program.funcs {
            let ty = self.word.fn_type(
                &vec![self.word.into(); f.params.len() + 1],
                false,
            );
            let fast = self.module.add_function(
                &format!("fun.f{}", f.name.0),
                ty,
                Some(Linkage::Internal),
            );
            fast.set_call_conventions(TAIL_CC);
            self.fast.push(fast);
            self.slow.push(self.module.add_function(
                &format!("fun.f{}.slow", f.name.0),
                self.word.fn_type(&[self.word.into(), ptr.into()], false),
                Some(Linkage::Internal),
            ));
        }
        for g in &self.program.globals {
            let value = self.module.add_global(
                self.word,
                None,
                &format!("fun.g{}", g.name.name.unique),
            );
            value.set_linkage(Linkage::Internal);
            value.set_initializer(&self.word.const_zero());
            self.globals.insert(g.name.clone(), value);
        }
    }
    /// Run the new pass manager only after constructing a complete, valid module.
    pub fn optimize(&self, level: u8) -> Result<()> {
        if level > 3 {
            return Err(Error("optimization level must be 0..3".into()));
        }
        self.module
            .run_passes(
                &format!("default<O{level}>"),
                &self.machine,
                PassBuilderOptions::create(),
            )
            .map_err(|e| Error(e.to_string()))?;
        self.verify()
    }
    /// Check SSA and ABI validity, preserving LLVM's diagnostic on failure.
    pub fn verify(&self) -> Result<()> {
        self.module.verify().map_err(|e| {
            Error(format!("internal error: LLVM verification: {e}"))
        })
    }
    /// LLVM emits an object; the separate native driver invokes the system linker.
    pub fn object(&self, path: &Path) -> Result<()> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| Error(e.to_string()))
    }
    /// Render the current module for driver dumps and snapshot tests.
    pub fn ir(&self) -> String {
        self.module.print_to_string().to_string()
    }
    fn block(&self, name: &str) -> BasicBlock<'ctx> {
        let f = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        self.context.append_basic_block(f, name)
    }
    fn entry(&self, f: FunctionValue<'ctx>) {
        self.builder
            .position_at_end(self.context.append_basic_block(f, "entry"));
    }
}
