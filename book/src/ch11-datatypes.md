# Chapter 11: Datatypes and Pattern Matching

> **New files**
>
> - `src/data.rs`
> - `src/syn.rs`
> - `src/desugar.rs`
>
> **Changed files**
>
> - `src/typecheck.rs`
> - `src/core.rs`
> - `src/elaborate.rs`
> - `src/ast.rs`
> - `src/core_lint.rs`
> - `src/core_type.rs`
> - `src/eval.rs`
> - `src/fixity.rs`
> - `src/layout.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/parse.rs`
> - `src/rename.rs`
> - `src/renamed.rs`
> - `src/session.rs`
> - `src/types.rs`

## Motivation

Integers and functions have taken us a long way, but it's time to build some
proper data structures. A binary tree can be empty or contain a value and two
subtrees. A lookup can fail or return a result. Algebraic datatypes let us say
these things directly, without inventing special values to mean “nothing here”.

Pattern matching gives us an equally direct way to take the values apart. The
shape of the data becomes the shape of the program. This is one of the beautiful
parts of functional programming, and we'll use it constantly from here on.

We'll first implement matching by trying patterns in source order. That gives
us a clear account of what a match means. In Chapter 16 we'll turn the same
matches into decision trees suitable for native code.

## The Language, Now

Declarations introduce nominal types and their constructors. Constructor arguments
are strict, and constructors can be passed around as curried functions.

```fun
data Tree a = Empty | Node (Tree a) a (Tree a)

size Empty = 0
size (Node left _ right) = 1 + size left + size right

main = size (Node Empty 42 Empty)
```

Read `size` beside the datatype. `Empty` contributes zero. A `Node` contributes
one, plus the sizes of its two children. The definition follows the data almost
word for word. That's beautiful, and it scales to much more interesting trees.

We can mirror a tree just as directly.

```fun
data Tree a = Empty | Node (Tree a) a (Tree a)

mirror Empty = Empty
mirror (Node left x right) = Node (mirror right) x (mirror left)

main = mirror (Node Empty 1 (Node Empty 2 Empty))
```

Notice that `mirror` never examines `x`. Its type is `Tree a -> Tree a`, so it
works for trees of any element type. The recursion changes the shape while
leaving the values alone.

Equations may have nested constructor patterns, literals, wildcards, and as-patterns.
Lists, pairs, triples, and unit have dedicated syntax. Guards must have type `Bool`.
If all guards fail, matching continues with the next equation.

```fun
select all@(x : xs) | x < 0 = xs
select all = all
main = select [-1,2,3]
```

A `where` block belongs to its entire equation, including every guard. A `case`
alternative may also have guards and a `where` block. Pattern lambdas and strict,
non-recursive pattern bindings work as well: `let (x,y) = (20,22) in x + y`.
Types remain first-order here. A datatype parameter has kind `*`, so `Tree Int` is
valid but applying a parameter as in `f Int` waits for Chapter 13.

The initial Prelude is embedded from this actual Fun source:

```fun
{{#include ../../crates/ch11_data/prelude.fun}}
```

## Theory

A datatype declaration supplies a polymorphic scheme for each constructor. For
example, `Node` has type `Tree a -> a -> Tree a -> Tree a`, quantified over `a`.
Each occurrence instantiates that scheme with fresh metavariables, just as a
polymorphic function occurrence does [@haskell2010; @pierce2002].

For a constructor with instantiated field types, a pattern judgment returns the
bindings introduced by its subpatterns:

$$
\frac{K : \tau_1 \to \tau_2 \to T \quad \Gamma \vdash p_1 : \tau_1 \Rightarrow \Delta_1 \quad \Gamma \vdash p_2 : \tau_2 \Rightarrow \Delta_2}{\Gamma \vdash K\;p_1\;p_2 : T \Rightarrow \Delta_1,\Delta_2}\;\text{(Constructor pattern)}
$$

The combined pattern environment must not bind a name twice. Each row is checked
against the same scrutinee types, and every successful right-hand side must have
the same result type. Guards additionally unify with `Bool`.

Fun evaluates fields before constructing a value. A failed match raises a
runtime error. There are no lazy patterns or suspended
constructor fields. Exhaustiveness and redundancy warnings require a separate
analysis. They arrive with match compilation [@maranget2007].

## Implementation

### Declaring names before checking fields

`data.rs` first allocates every type head, then converts constructor fields. This
permits mutually recursive datatypes. The immutable table stores each type's arity,
each constructor's scheme, and declaration-order tags. Duplicate names, unbound
field variables, and incorrect type arities are diagnosed before expression
inference begins.

The renamer resolves constructor names in expressions and patterns separately from
ordinary variables. Pattern variables receive fresh identities for each row. Its
scope then contains those variables and the row's local declarations while visiting
both guards and right-hand sides.

### Lowering syntax into Syn

`desugar.rs` introduces an explicit pass between renaming and inference. Lists and
tuples become constructor applications. `if` becomes a two-row match on `True` and
`False`. Infix `&&` and `||` first become conditionals. As first-class values those
operators are ordinary eta-expanded functions, whose arguments are evaluated by
call-by-value application.

A single equation containing only variable patterns can use ordinary lambdas and
lets. Other equations become a lambda for each argument followed by a match:

```rust
{{#include ../../crates/ch11_data/src/desugar.rs:equations-match}}
```

Strict pattern bindings use one checked bundle of their bound values, represented
by nested pairs, followed by projections. The right-hand side is evaluated once.
Its newly bound names are hidden while renaming that right-hand side, so an outer
binding of the same spelling remains accessible. An unused top-level pattern
binding still executes and can fail.

### Inferring constructor patterns

The checker instantiates a constructor scheme, separates its field arrows from
its result type, and unifies the result with the scrutinee. It then checks each
subpattern against the corresponding instantiated field:

```rust
{{#include ../../crates/ch11_data/src/typecheck.rs:constructor-pattern}}
```

Pattern variables enter the row environment monomorphically. Local bindings retain
ordinary let-polymorphism. The checker records expression and pattern-binder types
alongside the instantiation metadata already needed by System F elaboration.

### Extending Core and lint

Elaboration eta-expands constructor values into lambdas whose bodies are saturated
`Con` nodes. Strict lets evaluate match scrutinees once and in source order. Core
keeps nested patterns until the decision-tree compiler is introduced:

```rust
{{#include ../../crates/ch11_data/src/core.rs:nested-match}}
```

Lint independently checks constructor type arguments, field counts and field types.
For a constructor pattern it instantiates the declaration under the scrutinee's
type arguments, then checks each binder. Guards must be Boolean, and every row
must produce the declared match result. Malformed-Core tests exercise these checks
without asking the surface inferencer to validate their inputs.

### Executing rows

The interpreter evaluates constructor fields left to right. Matching already
evaluated values recursively tests constructors and literals, extending a private
row environment with variables and as-patterns:

```rust
{{#include ../../crates/ch11_data/src/eval.rs:match-pattern}}
```

A failed pattern discards that environment and tries the next row. A successful
pattern evaluates its `where` groups once, then tests guards in order. If all fail,
execution continues with the next row. Exhausting the rows reports
`fun: pattern match failure at FILE:LINE:COL` and exits with status 1.

The value printer uses constructor precedence, so a nested application prints as
`Just (Left 3)`. Lists and tuples use their surface notation. A character list
prints as `['a','b']`, while primitive strings keep double quotes.

## Running It

```text
$ cd crates/ch11_data
$ cargo run -q -- run tests/programs/data/tree.fun --lint
(True,False)
$ cargo run -q -- dump --pass=syn tests/programs/data/bool-data.fun
nonrec { not#9 = (lambda $match#11 (match $match#11 [ (True) -> False ] [ (False) -> True ])); }
nonrec { main#10 = (app (app (,) (app not#9 True)) (app not#9 False)); }
$ cargo test --test data
```

Guard fallthrough is worth experimenting with. Make the first pattern match
but its guard fail, then check that the next equation runs. Also try an unused
top-level pattern binding that fails. Strict evaluation means “unused” doesn't
make the match disappear. These examples exercise the parts that an ordinary
successful tree traversal might miss.

## Exercises

1. **Introductory.** define a binary tree mirror function and test it on an
   asymmetric tree.
2. **Intermediate.** Explain the type errors from a constructor pattern with too
   few fields and from two equations returning different types.
3. **Intermediate.** Write two guards that fall through to a later equation.
   Move their shared computation into `where` and inspect the Syn dump.
4. **Advanced.** Compare strict pattern binding with Haskell's ordinary pattern
   binding. Construct a program whose termination or error behavior differs.
5. **Advanced.** Design a lint test that swaps two differently typed constructor
   fields while preserving the field count.

## Further Reading

The Haskell Report specifies datatype declarations, patterns, guards, and local
scopes [@haskell2010]. Pierce develops the connection between data introductions
and eliminations [@pierce2002]. Maranget's papers explain the later steps: warning
analysis and decision-tree compilation [@maranget2007; @maranget2008].
