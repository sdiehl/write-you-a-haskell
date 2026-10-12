# Chapter 4: Simply Typed Lambda Calculus

> **New files**
>
> - `src/types.rs`
>
> **Changed files**
>
> - `src/ast.rs`
> - `src/eval.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/names.rs`
> - `src/normal.rs`

## Motivation

The preceding evaluator discovers a mistake such as `1 True` only when it
tries to call the integer. We can reject that expression before running it.
A type describes the values an expression can produce. A function type also
describes the arguments it accepts. Checking these descriptions is another
compiler pass, with the same source spans used by parsing diagnostics.

This chapter makes parameter types explicit. That choice keeps the checker
small enough to follow directly from its rules. Chapter 6 removes the annotations
and asks the compiler to infer polymorphic types. For now, every binding has
one simple type, even when introduced by `let`.

## The language, now

Types are `Int`, `Bool`, and arrows. Arrows associate right, so `Int -> Bool -> Int`
means `Int -> (Bool -> Int)`. An arrow on the left needs parentheses.

```fun
let choose = \(b : Bool) -> \(x : Int) -> \(y : Int) ->
  if b then x else y
in choose True 10 20
```

All lambda parameters require an annotation. Application, arithmetic, and
lexical scope retain their earlier meanings. The inherited lambda examples
have been annotated. Their outputs are unchanged. Three inherited runtime
type errors now produce `E0201` during checking. `BREAKING.md` lists those changes.

## Theory

A typing environment maps names to types. Reading a variable looks up its nearest
binding. A lambda extends that environment while checking its body. Application
checks its argument against the function's domain [@pierce2002].

$$
\frac{\Gamma, x : \tau \vdash e : \sigma}
{\Gamma \vdash (\mathsf{lambda}\ x : \tau . e) : \tau \to \sigma}
\;\text{(Abs)}
$$

$$
\frac{\Gamma \vdash f : \tau \to \sigma \quad \Gamma \vdash a : \tau}
{\Gamma \vdash f\ a : \sigma}\;\text{(App)}
$$

The condition of an `if` has type `Bool`. Both branches have the same type.
Arithmetic takes integers, while comparisons return booleans. A `let` checks its
right-hand side before extending the environment, so it remains non-recursive.
There is no implicit quantification: an identity annotated `Int -> Int` cannot
also accept `Bool`.

Two informal properties explain the value of these rules. _Preservation_ says
that a reduction step retains the term's type. The substitution lemma supplies
the main argument for beta reduction. _Progress_ says a closed, well-typed term
is a value or can take a step. Our partial division primitive requires one
qualification: a step may report a defined runtime error, such as divide by
zero. Types eliminate the dynamic shape errors, not every possible failure.
The tests check preservation on representative reductions. They are examples,
not a proof of the theorem.

## Implementation

`types.rs` represents arrows recursively and prints them with the required
parentheses. There are no metavariables or substitution maps yet.

```rust
{{#include ../../crates/ch04_stlc/src/types.rs:types}}
```

The grammar uses right recursion for the arrow's codomain. The AST's lambda
variant now stores a `Type` beside the parameter name, so a successfully parsed
lambda always has its annotation.

```text
{{#include ../../crates/ch04_stlc/src/parser.lalrpop:type_grammar}}
```

The checker mirrors the typing rules. Its environment is a stack, searched
from the innermost binding outward. A scope is popped even if checking its body
returns an error. Application first finds the function type, then checks the
argument, giving a useful error at the argument rather than at the whole call.

```rust
{{#include ../../crates/ch04_stlc/src/types.rs:infer}}
```

The small `expect` helper compares two simple types structurally. On a mismatch,
it reports both the expected and found types at the expression's byte span.
Ariadne renders the source excerpt. This standalone checker stops at its first
type error. The earlier lexical and scope passes still collect their errors.

The evaluator ignores annotations and retains the closure representation from
Chapter 3. Substitution, normal-order reduction, and de Bruijn conversion carry
types through their lambda cases. This lets the preservation tests check every
intermediate term, and keeps annotated terms distinct in nameless dumps.

## Running it

```text
$ cd crates/ch04_stlc
$ cargo run -q -- run tests/programs/typed-return.fun
42
$ cargo run -q -- dump --pass=types tests/programs/church.fun
Int
$ cargo run -q -- repl
STLC: :quit, :type EXPR, :parse EXPR, :trace EXPR.
stlc> :type \(f : Int -> Bool) -> \(x : Int) -> f x
(Int -> Bool) -> Int -> Bool
stlc> :quit
```

`check` checks without evaluating. Both execution engines check types first.
`:trace` also rejects ill-typed input before displaying reductions. A successful
check of `1 / 0` is expected: its type is `Int`, even though execution fails.

## Exercises

1. **Easy.** Derive the type of the `choose` example using the abstraction rule.
2. **Easy.** Explain why `(Int -> Bool) -> Int` needs its parentheses.
3. **Medium.** Write an application with a well-typed function and an argument of the wrong type. Check the diagnostic's span.
4. **Hard.** State the substitution lemma for this calculus, including the condition on the replacement term, and work through the lambda case.

## Further reading

Pierce gives the formal progress and preservation proofs [@pierce2002].
Damas and Milner motivate the next step beyond simple types:
principal type schemes and polymorphic `let` [@damas1982].
Takenobu Tani's Type Introduction Illustrated for Casual Haskellers
([pdf](https://takenobu-hs.github.io/downloads/type_introduction_illustrated.pdf)
/ [github](https://github.com/takenobu-hs/type-introduction-illustrated)) draws
the same ideas as pictures, from simple types up to the extensions GHC adds.
