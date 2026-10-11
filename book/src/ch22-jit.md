# Chapter 22: JIT and the REPL

> **New files**
>
> - `src/jit.rs`
> - `src/repl.rs`
> - `src/bin/fun.rs`
>
> **Changed files**
>
> - `src/llvm/mod.rs`
> - `src/llvm/control.rs`
> - `rt/src/heap.rs`
> - `src/session.rs`
> - `src/rename.rs`
> - `build.rs`
> - `src/core.rs`
> - `src/core_type.rs`
> - `src/elaborate.rs`
> - `src/erase.rs`
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

An object file lets a program outlive its compiler. A JIT makes the opposite
arrangement: generated code lives inside the compiler process. LLVM already
knows how to translate our module into machine instructions. The new problem is
keeping that code, its constants, and the values referring to it alive together.

A REPL makes this problem visible. Enter `saved = make 40`, then call `saved 2`
on the next line. The closure may contain both a captured integer and an address
in the previous input's code. Collecting its environment or releasing that code
would break a well-typed program. Each accepted definition therefore extends two
linked environments: one used to check source, and one holding executable values.

We retain the compiler pipeline and runtime ABI from Chapter 21. This chapter
adds a small execution boundary and a transactional command loop. The same
language now supports standalone binaries, interpreter runs, and interactive
native execution.

## The language, now

There is no new Fun syntax. The REPL accepts expressions and declarations.
Explicit braces put several declarations into one input. A file loaded with
`:load` can use ordinary multiline layout.

```fun
make x = \y -> x + y
saved = make 40
```

Entering `saved 2` prints `42`. A pure expression is wrapped in the Prelude's
`print`, so its type must have a `Show` instance. An `IO` expression executes its
action and discards the result. Inspect a function with `:t` rather than trying
to print it.

| Command                                 | Meaning                                                           |
| :-------------------------------------- | :---------------------------------------------------------------- |
| `:t EXPR`                               | Infer the principal qualified type. `:type` is an alias.          |
| `:k TYPE`                               | Infer the kind of a constructor or type application.              |
| `:core EXPR`, `:anf EXPR`, `:llvm EXPR` | Inspect an intermediate program without executing it.             |
| `:load FILE`                            | Replace definitions after successful checking and initialization. |
| `:reset`                                | Release retained code, roots, and definitions.                    |
| `:engine NAME`                          | Choose `jit`, `core`, `anf`, or `flat`. Preserve definitions.     |
| `:quit`                                 | Leave the session. `:q` is an alias.                              |

Loading a file does not execute its `main`. Enter `main` explicitly to run it.
Duplicate user definitions are errors, as in a source file. Reset or load a
replacement to change them. A compiler error leaves accepted definitions intact. Put Prelude overrides in
the first declaration block or a loaded file. Later inputs cannot change the
names visible to earlier definitions. A fixity must accompany its operator
definition in the same input.

## Theory

A closure is a pair of code and environment, even when our representation stores
several captured fields [@marlow2004]. Reachability of the environment is only
half its lifetime condition: the code address must remain executable too. The
Boehm collector understands heap reachability, but does not own LLVM's machine
code or know when an execution engine can be discarded [@boehm1988].

We give a session ownership of all its execution engines. Each engine owns one
input's module. A separate table holds global words in registered root slots.
Earlier modules can therefore remain immutable, and later modules import their
values through external global declarations. Reset destroys the engines before
unregistering the slots. The enclosing LLVM Context outlives both.

The source environment follows a transactional rule. Checking a candidate may
construct types, evidence, and IR, but does not publish the candidate. Only after
successful preparation and execution do we retain its definitions. Inspection runs only the static passes. It must not initialize globals or
execute an action. In particular, inspecting the type of `error "x"`
is safe. Evaluating it is not. The boundary between static reasoning and
execution remains the one established by the typed compiler [@pierce2002].

## Implementation

### A returning entry

A native executable starts with `main`, initializes globals, executes the IO
action, and exits. A REPL cannot exit after every successful input. JIT lowering
instead emits a C-callable `void fun_entry(void)`: initialize new globals,
apply the query action to unit, then return. Fast entries still use `tailcc`.
Closure calls still use the same C eval/apply path. Calls across that ABI
boundary are explicitly `notail`. A wide-call regression caught LLVM inferring
a tail transfer from a `tailcc` function into C apply, which corrupted the
callee-pop return frame on AArch64. Verification did not catch this target-level
failure. Executing the same program on both backends did. The fix also applies
to Chapter 21.

`Backend::for_jit` selects this entry and external global storage. It reuses
Chapter 21's worklist emission, verifier, and pass manager. We run O1 for the
interactive path. Native `build` retains its O0–O3 choice.

```rust
#![function!("../../crates/ch22_jit/src/llvm/mod.rs", Backend::for_jit)]
```

### Resolve symbols before execution

The system linker resolves a native object's runtime calls. MCJIT needs the
same addresses inside the compiler. `runtime_symbols` takes the addresses of
the linked runtime's C exports, keeping them in the binary. `map_symbols` checks
every external function and installs its address with `add_global_mapping`.
Earlier fast and slow entries are mapped to their retained engines in the same
step. LLVM intrinsics remain LLVM's responsibility.

```rust
#![function!("../../crates/ch22_jit/src/jit.rs", map_symbols)]
```

An unrecognized import is an error before execution. We do not depend on the
host executable exporting symbols by name. String operations, formatting,
application, errors, and IO all reach the Chapter 22 runtime rlib, whose ABI
matches the static library used by native executables.

### Keep code and roots together

The session's ownership is explicit:

```rust
#![struct!("../../crates/ch22_jit/src/jit.rs", Jit)]
```

A global slot uses the runtime's `GlobalRoot` owner. Its boxed storage stays
at a stable address and is registered with Boehm. `UnsafeCell` permits generated
code to write the slot without conflicting with Rust's reference rules. Mapping an LLVM global to that
address makes both generated loads and the collector see the same word. A Rust
container alone would not provide this guarantee: Boehm does not scan Rust's
ordinary heap transitively.

The source group order distinguishes Prelude globals from user globals that
shadow their spelling. Session-local renamer uniques identify LLVM declarations
within a compilation, but cannot serve as persistent keys: adding declarations
may change their numbers. Accepted slots are reused and their initializers are
skipped. Before importing a value, an alpha-normalized type fingerprint checks that
rechecking has not changed its type, and the saved syntactic arity checks the
fast entry's argument count. Erasure preserves this flat metadata. The
fingerprint traversal uses an explicit worklist. Query slots are fresh and retained with their engines, so no surviving
relocation points into freed storage.

`Jit::run` finishes all mappings before retrieving a typed callable. Its unsafe
call is justified by a checked contract: the backend emits exactly the C
signature requested from Inkwell, all runtime imports are resolved, and the
engine and roots remain alive throughout the call. The callable is used
immediately. We never add code to that engine afterward. Inkwell documents that
later additions can invalidate a previously retrieved function pointer.

```rust
#![function!("../../crates/ch22_jit/src/jit.rs", call_entry)]
```

### Commit definitions after success

`Repl::line` dispatches commands in a loop. Declarations are added to a candidate
source list, checked with a private `$repl` entry, and initialized before the
list replaces the accepted one. The private name cannot be written in Fun and
does not collide with a loaded `main`. A load similarly constructs a replacement
session before swapping it in.

Expression execution uses ordinary language operations:

```rust
#![function!("../../crates/ch22_jit/src/repl.rs", action_query)]
```

The resulting query passes through inference and elaboration again. The wrapper
does not bypass class solving or IO abstraction. Type, kind, and IR commands
stop before execution. All new traversal and command control uses loops. No
larger Rust stack or stack-growth library is introduced. Recursive frontend
passes inherited from earlier chapters retain their existing limits.

We recheck accepted source and lower a candidate module on each input. Existing
global values and fast/slow function entries are imported, preserving both
initialization and code identity. Unused regenerated local bodies are removed
by optimization, but compilation
work and retained machine code grow with the session. This makes ownership easy
to inspect. Incremental checking and code reclamation would require additional
dependency and lifetime machinery.

> **Note.** Runtime errors and `exitWith` retain native process-exit semantics,
> including in the JIT REPL. We never unwind or jump across generated frames.
> Compiler errors are recoverable. If creation of the execution engine fails,
> the REPL selects Flat. Verifier failures are reported rather than hidden by
> fallback. Explicit `run --engine=jit` reports setup failure.

## Running it

Build with the LLVM 22 and Boehm setup from Chapter 21:

```text
$ python3 scripts/build_runtime.py 22
$ cargo build -p ch22_jit
$ target/debug/fun repl --lint
Fun: JIT ready on first input. Type :help for commands.
fun> make x = \y -> x + y
fun> saved = make 40
fun> saved 2
42
fun> :t saved
saved : Int -> Int
fun> :k Maybe
Maybe : * -> *
fun> :reset
Reset.
fun> :quit
$ target/debug/fun run crates/ch22_jit/tests/programs/native/acceptance.fun --engine=jit --lint
5.0
3.0
6.0
[3.0,10.0]
True
50000005000000
$ target/debug/fun dump --pass=llvm crates/ch22_jit/tests/programs/jit/returned-function.fun > build/jit-example.ll
```

`fun` and `fun-ch22` share one driver. The CLI keeps Flat as its default `run`
engine. The REPL defaults to JIT. `--fuel` and the test-only `--print-value`
remain interpreter facilities. Redirected REPL input omits prompts and the
banner so transcripts can be compared byte for byte.

The 422-program corpus now includes JIT execution alongside Core, ANF, Flat, and
native binaries, subject to each fixture's engine header. The 1,000 generated
integer programs use all five. Separate JIT checks cover the ten-million-call
acceptance and mutual-recursion programs and allocation stress under a bounded
Boehm heap. Known calls retain Chapter 21's tail-call guarantee. Unknown calls
through C apply retain its limitation.

The REPL tests check persistence, loaded actions, type and kind queries, engine
switches, reset, and recovery from rejected input. A lifetime regression forces
collections between modules, verifies that saved global addresses stay unchanged,
and calls an earlier captured closure from later code. Those tests exercise the
ownership argument directly, beyond LLVM's structural verification.

## Exercises

1. **Warm-up:** trace the owners of `saved = make 40` across two inputs. Identify
   which owner protects its environment and which protects its code address.
2. **Intermediate:** remove one runtime mapping and inspect the failure. Explain
   why it must be detected before calling the generated entry.
3. **Intermediate:** add a regression where a user definition shadows a Prelude
   name. Show why spelling alone cannot identify its persistent slot.
4. **Advanced:** design an isolated worker protocol that recovers from runtime
   errors while retaining accepted definitions. Specify what must be replayed.
5. **Advanced:** propose a module-reclamation scheme. Account for closures inside
   constructors, partial applications, global values, and captured environments.

## Further reading

- [Inkwell ExecutionEngine documentation](https://thedan64.github.io/inkwell/inkwell/execution_engine/struct.ExecutionEngine.html),
  particularly symbol mappings and the safety contract of `get_function`.
- [Compiler Crates: Inkwell](https://sdiehl.github.io/compiler-crates/inkwell.html),
  for LLVM construction through the Rust bindings.
- Closures, partial applications, and slow entries [@marlow2004].
- Conservative roots and cooperation with a runtime [@boehm1988].
- Typing and the boundary before evaluation [@pierce2002].
