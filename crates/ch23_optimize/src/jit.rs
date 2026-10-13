//! An engine owns executable code; registered Rust slots own its global roots.
use crate::{flat, llvm::Backend};
use fun_rt::heap::GlobalRoot;
use inkwell::{
    OptimizationLevel, context::Context, execution_engine::ExecutionEngine,
};
use std::collections::BTreeMap;

// A Prelude binding and a source binding may share a spelling. Source-group
// order distinguishes them without relying on session-local unique numbers.
type GlobalKey = (String, usize);

/// Keep every engine alive until no REPL binding can reference its closures.
/// Fields drop in declaration order: release code before unregistering the slots.
pub struct Jit<'ctx> {
    engines: Vec<ExecutionEngine<'ctx>>,
    roots: BTreeMap<GlobalKey, GlobalRoot>,
    queries: Vec<GlobalRoot>,
    code: BTreeMap<GlobalKey, (usize, usize, usize)>,
    types: BTreeMap<GlobalKey, Vec<String>>,
    context: &'ctx Context,
}

impl<'ctx> Jit<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        fun_rt::rt_init();
        Self {
            engines: vec![],
            roots: BTreeMap::new(),
            queries: vec![],
            code: BTreeMap::new(),
            types: BTreeMap::new(),
            context,
        }
    }
    /// Compile a checked program, import accepted globals, then initialize only new bindings.
    /// Runtime errors retain the native runtime's process-exit semantics.
    pub fn run(&mut self, program: &flat::Program) -> Result<(), String> {
        let mut occurrences = BTreeMap::new();
        let mut keys = BTreeMap::new();
        for global in &program.globals {
            let count =
                occurrences.entry(global.name.text.clone()).or_insert(0);
            keys.insert(
                global.name.clone(),
                (global.name.text.clone(), *count),
            );
            *count += 1;
        }
        let mut checked_types = BTreeMap::new();
        for (id, shape) in &program.metadata.global_types {
            let Some(key) = keys.get(id) else { continue };
            if let Some(previous) = self.types.get(key)
                && previous != shape
            {
                return Err(format!(
                    "input changes the accepted type of {}; use :reset or :load",
                    key.0
                ));
            }
            checked_types.insert(key.clone(), shape.clone());
        }
        let inherited = keys
            .iter()
            .filter(|(_, key)| self.roots.contains_key(*key))
            .map(|(id, _)| id.clone())
            .collect();
        let backend = Backend::for_jit(self.context, program, inherited)
            .map_err(|e| e.to_string())?;
        backend.optimize(1).map_err(|e| e.to_string())?;
        let engine = backend
            .module
            .create_jit_execution_engine(OptimizationLevel::Less)
            .map_err(|e| format!("JIT setup failed: {e}"))?;
        let mut imports = vec![];
        for global in &program.globals {
            if let Some(f) = global.function
                && let Some((fast, slow, arity)) =
                    self.code.get(&keys[&global.name])
            {
                if *arity != program.funcs[f.0 as usize].params.len() {
                    return Err(format!(
                        "input changes the accepted arity of {}; use :reset or :load",
                        global.name.text
                    ));
                }
                imports.push((format!("fun.f{}", f.0), *fast));
                imports.push((format!("fun.f{}.slow", f.0), *slow));
            }
        }
        map_symbols(&backend.module, &engine, &imports)?;
        let mut pending = BTreeMap::new();
        for global in &program.globals {
            let name = &keys[&global.name];
            let slots = if let Some(slots) = self.roots.get(name) {
                slots
            } else {
                pending
                    .entry(name.clone())
                    .or_insert_with(|| GlobalRoot::new(0))
            };
            let value = backend
                .module
                .get_global(&format!("fun.g{}", global.name.name.unique));
            let Some(value) = value else { continue }; // O1 removed an unused declaration.
            engine.add_global_mapping(&value, slots.as_ptr() as usize);
        }
        let mut pending_code = BTreeMap::new();
        for global in &program.globals {
            let key = &keys[&global.name];
            if let Some(f) = global.function
                && !self.code.contains_key(key)
            {
                let fast = engine
                    .get_function_address(&format!("fun.f{}", f.0))
                    .map_err(|e| e.to_string())?;
                let slow = engine
                    .get_function_address(&format!("fun.f{}.slow", f.0))
                    .map_err(|e| e.to_string())?;
                pending_code.insert(
                    key.clone(),
                    (fast, slow, program.funcs[f.0 as usize].params.len()),
                );
            }
        }
        call_entry(&engine)?;
        // A transient entry gets a fresh slot on every input; retain even that slot
        // while its module exists, so no relocation can ever address freed storage.
        if let Some(query) =
            pending.remove(&keys[&program.metadata.main.name])
        {
            self.queries.push(query);
        }
        let query = &keys[&program.metadata.main.name];
        pending_code.remove(query);
        checked_types.remove(query);
        self.code.append(&mut pending_code);
        self.types.append(&mut checked_types);
        self.roots.append(&mut pending);
        self.engines.push(engine);
        Ok(())
    }
    /// Introspection for tests: accepted globals survive later inputs without reinitialization.
    pub fn global_word(&self, name: &str) -> Option<u64> {
        self.roots
            .iter()
            .rfind(|((text, _), _)| text == name)
            .map(|(_, root)| root.get())
    }
}

/// One explicit symbol table links the JIT to the very same ABI as native objects.
pub fn runtime_symbols() -> Vec<(&'static str, usize)> {
    macro_rules! entries {
        ($module:path => [$($name:ident),* $(,)?]) => {{
            use $module as runtime;
            vec![$((stringify!($name), runtime::$name as *const () as usize)),*]
        }};
    }
    let mut symbols = entries!(fun_rt => [
        rt_init,
        rt_alloc,
        rt_alloc_atomic,
        rt_apply,
        rt_apply1,
        rt_apply2,
        rt_apply3,
        rt_apply4,
        rt_closure_set,
        rt_make_pap,
        rt_div_zero,
        rt_error,
        rt_exit,
        rt_pattern_fail,
    ]);
    symbols.extend(entries!(fun_rt::abi => [
        rt_prim_intAdd,
        rt_prim_intSub,
        rt_prim_intMul,
        rt_prim_intNeg,
        rt_prim_intQuot,
        rt_prim_intRem,
        rt_prim_intDiv,
        rt_prim_intMod,
        rt_prim_intEq,
        rt_prim_intLt,
        rt_prim_intLe,
        rt_prim_dblAdd,
        rt_prim_dblSub,
        rt_prim_dblMul,
        rt_prim_dblDiv,
        rt_prim_dblNeg,
        rt_prim_dblSqrt,
        rt_prim_dblExp,
        rt_prim_dblLog,
        rt_prim_dblSin,
        rt_prim_dblCos,
        rt_prim_dblPow,
        rt_prim_dblEq,
        rt_prim_dblLt,
        rt_prim_dblLe,
        rt_prim_intToDbl,
        rt_prim_dblFloor,
        rt_prim_dblRound,
        rt_prim_dblTruncate,
        rt_prim_chrOrd,
        rt_prim_chrChr,
        rt_prim_chrEq,
        rt_prim_chrLt,
        rt_prim_strAppend,
        rt_prim_strLength,
        rt_prim_strEq,
        rt_prim_strCompare,
        rt_prim_strUnpack,
        rt_prim_strPack,
        rt_prim_showInt,
        rt_prim_showDouble,
        rt_prim_showChar,
        rt_prim_showString,
        rt_prim_readInt,
        rt_prim_error,
        rt_prim_putStr,
        rt_prim_getLine,
        rt_prim_exit,
        rt_prim_trace,
    ]));
    symbols
}

/// Resolve declarations before MCJIT finalizes the module. Intrinsics belong to LLVM.
fn map_symbols(
    module: &inkwell::module::Module<'_>,
    engine: &ExecutionEngine<'_>,
    imports: &[(String, usize)],
) -> Result<(), String> {
    let symbols = runtime_symbols();
    for function in module
        .get_functions()
        .filter(|f| f.count_basic_blocks() == 0)
    {
        let name =
            function.get_name().to_str().map_err(|e| e.to_string())?;
        if name.starts_with("llvm.") {
            continue;
        }
        let address = symbols
            .iter()
            .find(|(symbol, _)| *symbol == name)
            .map(|(_, address)| *address)
            .or_else(|| {
                imports
                    .iter()
                    .find(|(symbol, _)| symbol == name)
                    .map(|(_, address)| *address)
            })
            .ok_or_else(|| {
                format!("unmapped JIT runtime symbol: {name}")
            })?;
        engine.add_global_mapping(&function, address);
    }
    Ok(())
}

/// Enter a finalized module without exposing a callable to the rest of the compiler.
fn call_entry(engine: &ExecutionEngine<'_>) -> Result<(), String> {
    // SAFETY: the verified backend emits exactly void fun_entry(void), C ABI.
    // All imports are explicitly mapped; code, constants, and root slots remain live.
    // This engine is never modified after lookup, and no pointer escapes this call.
    unsafe {
        let entry = engine
            .get_function::<unsafe extern "C" fn()>("fun_entry")
            .map_err(|e| e.to_string())?;
        entry.call();
    }
    Ok(())
}
