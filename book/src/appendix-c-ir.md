# Appendix C: Intermediate Representations

The final compiler has three executable intermediate languages. Each removes a
specific obligation from the next pass. These definitions are included from
Chapter 24, so the reference and implementation stay together.

| Representation | Makes explicit                                            | Still leaves implicit                           |
| :------------- | :-------------------------------------------------------- | :---------------------------------------------- |
| System F Core  | Types, dictionaries, constructors, control flow           | Evaluation temporaries and closure environments |
| ANF            | Evaluation order, intermediate results, joins, tail calls | Captured variables and application dispatch     |
| Flat           | Lifted code, closure fields, known and unknown calls      | Physical storage and machine instructions       |

## Typed Core

A value identifier carries its type. A type application instantiates a type
abstraction. Dictionary arguments are ordinary value arguments, so erasing
`TyLam` and `TyApp` does not erase class evidence.

```rust
#![struct!("../../crates/ch24_gc/src/core.rs", Id)]
#![enum!("../../crates/ch24_gc/src/core_type.rs", Type)]
#![enum!("../../crates/ch24_gc/src/core.rs", Expr)]
```

`Match` belongs to elaborated Core. Match compilation removes it, leaving flat
`Case` alternatives and lexical `Join`/`Jump`. Constructors and primitives are
saturated. A jump is fully applied and appears only in tail position within the
scope of its join. The lint pass checks these properties independently of the
source type checker.

```rust
#![enum!("../../crates/ch24_gc/src/core.rs", AltCon)]
#![struct!("../../crates/ch24_gc/src/core.rs", Alt)]
#![struct!("../../crates/ch24_gc/src/core.rs", Bind)]
#![struct!("../../crates/ch24_gc/src/core.rs", BindGroup)]
```

A recursive group contains functions. A nonrecursive group's right-hand sides
are evaluated in dependency order. Case binders name the already evaluated
scrutinee. Pattern failure and failing primitives retain source locations.

## A-Normal Form

Atoms cannot perform an effect or fail when read from a well-scoped environment.
A `Let` names one strict computation. The body type is `Rc<Anf>`, allowing the
machine and transformation worklists to share immutable syntax.

```rust
#![enum!("../../crates/ch24_gc/src/anf.rs", Atom)]
#![struct!("../../crates/ch24_gc/src/anf.rs", Lambda)]
#![enum!("../../crates/ch24_gc/src/anf.rs", Rhs)]
#![enum!("../../crates/ch24_gc/src/anf.rs", Anf)]
```

`Ret` returns to the current continuation. `TailCall` replaces the current
activation. `Jump` stays inside it. A non-tail source case sends its alternatives
to one join representing the rest of the computation. The iterative ANF machine
uses this distinction to run deep tail recursion without recursive Rust calls.

## Flat IR

Every lambda has become a lifted function. Its environment is explicit when it
captures values. Function indices refer to the program's function table.

```rust
#![struct!("../../crates/ch24_gc/src/flat.rs", Func)]
#![struct!("../../crates/ch24_gc/src/flat.rs", GlobalInit)]
#![struct!("../../crates/ch24_gc/src/flat.rs", Program)]
#![enum!("../../crates/ch24_gc/src/flat.rs", Rhs)]
#![enum!("../../crates/ch24_gc/src/flat.rs", Flat)]
```

Known calls name a lifted function with the correct number of arguments. Unknown
calls use eval/apply, which supports exact, partial, and oversaturated calls.
`PatchClosure` initializes a recursive capture after allocating every closure in
the group. It is an internal operation, never a source mutation primitive.

## Numeric Evidence and Storage

Chapter 23 preserves a small classification derived from checked Core types:

```rust
#![enum!("../../crates/ch24_gc/src/rep.rs", Rep)]
```

This is type evidence, not a calling convention. Call results and parameters
remain words even when their evidence says `Int` or `Double`. LLVM's local
locations distinguish words, native numbers, and reloadable roots:

```rust
#![enum!("../../crates/ch24_gc/src/llvm/values.rs", Loc)]
```

The copying backend reloads a `Root` after a possible collection. Arithmetic can
keep a native number in a register until a boundary requires tagging or boxing.
The [runtime ABI](appendix-d-runtime.md) specifies those boundaries.
