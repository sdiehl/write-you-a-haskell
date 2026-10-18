# Chapter 9: The Renamer

> **New files**
>
> - `src/rename.rs`
> - `src/scc.rs`
> - `src/renamed.rs`
>
> **Changed files**
>
> - `src/names.rs`
> - `src/typecheck.rs`
> - `src/ast.rs`
> - `src/core.rs`
> - `src/elaborate.rs`
> - `src/eval.rs`
> - `src/fixity.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/parse.rs`
> - `src/types.rs`

## Motivation

Two variables can both be called `x` and mean entirely different things. That's
easy enough to follow in a small expression. It gets less comfortable when the
compiler starts moving expressions around. We'd like to settle which binding
each occurrence means once, then carry that answer through the rest of the
compiler.

That's the renamer's first job. Its second is to free us from writing helpers
before the functions that use them. Some functions even call each other, so
there is no possible ordering that puts every helper first. We'll find those
groups in a dependency graph and give the type checker the whole group at once.

## The Language, Now

Top-level and local declarations may refer forward within their block. These two
functions belong to the same recursive component:

```fun
even n = if n == 0 then True else odd (n - 1)

odd n = if n == 0 then False else even (n - 1)

main = even 100
```

Each binding block is one lexical scope. A local definition shadows a global
with the same spelling. Two definitions in the same block are an `E0102` error.
Every member of a recursive component must have a function right-hand side.
Thus `a = b; b = a` is rejected with `E0120`, even if neither value is used.

Fixities also receive a scope check. A top-level fixity declaration must have a
matching definition in the file (`E0130`). An unbound variable reports `E0101`
and suggests an in-scope spelling when its edit distance is at most two.

## Theory

A binding identity consists of an interned spelling and a fresh integer. The
spelling serves diagnostics. The unique integer separates shadowed binders.
Inference and Core use these identities, so neither has to repeat lexical lookup.
This is the compiler counterpart of the binding distinctions explored in
Chapter 3 [@pierce2002].

Draw an arrow from a function to each binding in its block that it uses. If
`even` calls `odd` and `odd` calls `even`, we can follow arrows from either one
back to the other. They belong together. A _strongly connected component_ is a
maximal group with this mutual-reachability property.

Now treat each group as one node. The resulting graph has no cycles, so we can
process its dependencies first. That's the order our type checker needs. A
helper outside a recursive group can have its type generalized before its callers
use it [@haskell2010].

Let's add `id x = x` and `main = id (even 4)` to the example. The dependency
arrows are `main → id`, `main → even`, `even → odd`, and `odd → even`.
We can check `id` and the `even`/`odd` group independently, but both must be
ready before `main`. We generalize `id` as soon as its own group is finished.
Grouping every declaration together would unnecessarily make its uses share
one provisional type while checking the whole block.

Inside a recursive component, allocate one provisional monotype per binder and
infer all bodies against them. Unify those provisional types with the results.
Only then generalize the component. Outside the component, each occurrence can
instantiate the resulting schemes independently. Signatures still check inferred
types here. Signature-directed polymorphic recursion remains Chapter 14's work.

## Implementation

### Name resolution

The surface tree retains a whole local declaration block until renaming. The
renamer allocates all of that block's identities before visiting any right-hand
side. Each lambda introduces a smaller scope containing its parameter. Lookup
searches scopes from the innermost outward.

```rust
{{#include ../../crates/ch09_rename/src/rename.rs:rename}}
```

The renamed tree is a separate IR. Its occurrences contain an `Id`, which keeps a
`Name` and its diagnostic spelling together. Dumps display names as `x#12`.
This small amount of repeated text keeps dumps readable after the source session
has gone away. Comparisons still include the allocated identity.

Errors accumulate during the walk. An unresolved occurrence receives a fresh
placeholder so other bodies can still be checked for scope errors. The driver
stops after the renaming pass, before any placeholder can reach evaluation.

### Dependency components

Tarjan's search assigns each vertex an index and records the lowest reachable
index still active on the search stack. When the two indices agree, the vertex
is the root of a complete component, which can be popped together.

```rust
{{#include ../../crates/ch09_rename/src/scc.rs:tarjan}}
```

Our edges point toward dependencies, so completed components arrive in the
order inference needs. We visit source indices in a fixed order to keep dumps
reproducible. A group containing just one binding is recursive only if that
binding refers to itself. As a check on Tarjan's bookkeeping, the tests also
find components by the much slower but direct method of testing mutual
reachability.

### Inference by group

The environment contains all provisional types while a recursive component is
being inferred. Generalization waits until every body has contributed constraints.
Signatures are checked against the inferred schemes, and specialization propagates
through the shared component before the exported schemes are finalized.

```rust
{{#include ../../crates/ch09_rename/src/typecheck.rs:group}}
```

That last step matters. If `f` and `g` call one another and `f` has an `Int -> Int`
signature, a shared argument type must also constrain `g`. Exporting an unconstrained
polymorphic scheme for `g` would be unsound. A regression test rejects a subsequent
`g True` call.

### Recursive closures

The interpreter stores an immutable recipe for every recursive group, containing its lambda
bodies and the outer environment. Entering one of these closures reconstructs the
group's bindings before adding the function argument. No closure needs a mutable
pointer to itself, and there is no reference-counting cycle.

```rust
{{#include ../../crates/ch09_rename/src/eval.rs:recursive-closures}}
```

Top-level value initializers run once in dependency order. Functions are closures.
Creating one does not run its body. Core lint now checks unique binder identities
as well as scope, primitive saturation, and the recursive-lambda restriction.

## Running It

```text
$ cd crates/ch09_rename
$ cargo run -q -- run tests/programs/rename/mutual.fun
True
$ cargo run -q -- dump --pass=rename tests/programs/fun-forward.fun
nonrec { later#10 = 1; }
nonrec { main#9 = later#10; }
$ cargo xtask check-inherit 09
chapter 09: 101 inherited programs verified and chapter tests passed
```

Try moving the helper in a small program below its caller. It now works.
Then make the two functions call each other and inspect the rename dump.
You should see one recursive group, which tells the checker to infer them
together.

## Exercises

1. **Introductory.** annotate each occurrence of `x` in a nested-shadowing example
   with the binder it references. Compare with the rename dump.
2. **Intermediate.** Draw the dependency graph of a three-function recursive group
   with one non-recursive helper, then predict the emitted component order.
3. **Intermediate.** Add a diagnostic test with two independent misspelled names
   and verify that the pass reports both.
4. **Advanced.** Explain why the immutable closure recipe has no reference-counting
   cycle even when one of its functions returns another closure.
5. **Advanced.** Identify where signature-directed polymorphic recursion will
   require the current component construction and checking strategy to change.

## Further Reading

The Haskell Report describes binding groups and dependency analysis [@haskell2010].
Pierce supplies the underlying treatment of variables, scope, and binding
[@pierce2002].
