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

Our evaluator will happily try to run `1 True`. It eventually discovers that
an integer isn't a function, but we didn't need to run the program to spot
that mistake. Could the compiler catch it for us?

A type checker does exactly this sort of reasoning. A function type tells us
what a function accepts and what it returns. We check a call by comparing those
types with the argument we're about to pass. The rules are small enough to
write down, and the Rust implementation will follow them closely.

For now we'll ask the programmer to annotate lambda parameters. This gives us
a clear starting point. In Chapter 6 we'll teach the compiler to work out types
for itself, including types that work for many different kinds of values.

## The Language, Now

Types are `Int`, `Bool`, and arrows. Arrows associate right, so `Int -> Bool -> Int`
means `Int -> (Bool -> Int)`. An arrow on the left needs parentheses.

```fun
let choose = \(b : Bool) -> \(x : Int) -> \(y : Int) ->
  if b then x else y
in choose True 10 20
```

We annotate every lambda parameter, but calls, arithmetic, and scope work as
before. Try calling an integer as a function again. This time `E0201` comes from
the checker, before the evaluator gets involved. We've moved the discovery of
the mistake earlier in the process.

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
There is no implicit quantification. An identity annotated `Int -> Int` cannot
also accept `Bool`.

Two useful ideas explain what we've gained. _Preservation_ says that taking an
evaluation step doesn't change an expression's type. If we start with an `Int`
expression, reducing it shouldn't suddenly produce a function. The main proof
step for a lambda call shows that substituting a correctly typed argument keeps
the body's type intact. This is the substitution lemma.

_Progress_ says that a closed, well-typed expression is either a value or can
take a step. We won't reach a nonsensical instruction such as calling `42`.
Our division primitive needs a small qualification, since division by zero can
still report a runtime error. Types catch this language's shape mistakes. They
don't promise that every computation succeeds. Pierce develops the proofs
behind these claims [@pierce2002].

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

## Running It

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
check of `1 / 0` is expected. Its type is `Int`, even though execution fails.

## Exercises

1. **Easy.** Derive the type of the `choose` example using the abstraction rule.
2. **Easy.** Explain why `(Int -> Bool) -> Int` needs its parentheses.
3. **Medium.** Write an application with a well-typed function and an argument of the wrong type. Check the diagnostic's span.
4. **Hard.** State the substitution lemma for this calculus, including the condition on the replacement term, and work through the lambda case.

## Further Reading

Pierce gives the formal progress and preservation proofs [@pierce2002].
Damas and Milner motivate the next step beyond simple types:
principal type schemes and polymorphic `let` [@damas1982].
Takenobu Tani's Type Introduction Illustrated for Casual Haskellers
([pdf](https://takenobu-hs.github.io/downloads/type_introduction_illustrated.pdf)
/ [github](https://github.com/takenobu-hs/type-introduction-illustrated)) draws
the same ideas as pictures, from simple types up to the extensions GHC adds.
