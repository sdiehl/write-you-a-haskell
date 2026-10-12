# Chapter 3: Lambda Calculus

> **New files**
>
> - `src/names.rs`
> - `src/normal.rs`
>
> **Changed files**
>
> - `src/eval.rs`
> - `src/ast.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`

## Motivation

Our calculator can name a number. What if we could name a computation and use
it with different inputs? That's a function, and adding functions takes us a
surprisingly long way. With just lambdas and application we can express the
pure untyped lambda calculus. Even data can be represented by functions, as
we'll see with Church encodings [@pierce2002]. That's a remarkable amount of
expressive power for two pieces of syntax!

There are a couple of details we need to get right. If two variables are both
called `x`, how do we know which one a function means? And when we call a
function, should we evaluate its argument first? We'll build substitution and
closures ourselves so we can see the answers, rather than quietly borrowing
Rust's answers.

## The Language, Now

Programs remain single expressions. Lambdas have one parameter, application
associates left, and booleans control an `if` expression. Comparisons are integer
operations. There are no type annotations yet.

```fun
let twice = \f -> \x -> f (f x) in twice (\x -> x + 1) 0
```

A closure remembers the bindings present where its lambda was evaluated. In
`let x = 1 in let f = \y -> x + y in let x = 100 in f 2`, the result is 3.
Using the caller's binding of `x` instead would give dynamic scope.

## Theory

Calling a lambda means replacing its parameter with the supplied argument.
This step is called _beta reduction_. For `\x -> x + 1` applied to `41`, we
replace `x` and get `41 + 1`. So far, so good. The subtlety begins when names
repeat. We replace only _free_ occurrences of the parameter in the body, and
rename an inner binder if it would capture a free name from the argument.
We'll work through exactly that mistake below [@pierce2002].

Evaluation order decides which reduction we do next. Normal order takes the
leftmost outermost reducible expression, and our full reducer also continues
under lambdas. Call-by-value first evaluates the function and its argument,
then enters the body. A lambda itself is already a value.

The difference becomes visible when a function ignores an argument that would
run forever. Normal order can skip that argument. Call-by-value gets stuck
trying to compute it. The Y combinator gives us a particularly striking example.
It computes factorial under normal order but exhausts our strict evaluator's
fuel. Running out of fuel means we've reached our experiment's limit, not that
we've proved the computation will never finish.

### Why De Bruijn Indices?

Names help us read programs, but their spelling is incidental to binding.
`\x -> x` and `\answer -> answer` describe the same function. We call them
_alpha-equivalent_. Renaming a bound variable consistently changes nothing.
Comparing their syntax trees directly, however, finds different strings.
A compiler that rewrites named terms must keep distinguishing harmless renaming
from a change in which binder an occurrence refers to.

Consider this reduction of an open term, where `y` is free:

```text
(\x -> \y -> x) y
```

Replacing `x` with `y` mechanically gives `\y -> y`. That is wrong. The function
should return the free `y` supplied as its argument, regardless of its own
parameter. Rename the inner parameter first:

```text
(\x -> \z -> x) y
→ \z -> y
```

This is the capture problem our substitution function must solve. De Bruijn
indices give binding a representation that does not depend on fresh spellings.
Replace each bound occurrence with its distance to its binder, counting from
zero. Count enclosing binders, not application nodes or source lines.

We write `λ. body` for an unnamed lambda in the examples below. Its numbers are
variable indices, not integer literals. This is explanatory notation, not input
syntax for the chapter's parser.

| Named term        | Indexed term | Reading                                     |
| ----------------- | ------------ | ------------------------------------------- |
| `\x -> x`         | `λ. 0`       | Return the nearest parameter.               |
| `\x -> \y -> x`   | `λ. λ. 1`    | Skip `y` to reach `x`.                      |
| `\x -> \y -> y`   | `λ. λ. 0`    | Return `y`.                                 |
| `\f -> \x -> f x` | `λ. λ. 1 0`  | Apply the outer parameter to the inner one. |
| `\x -> \x -> x`   | `λ. λ. 0`    | The inner `x` shadows the outer one.        |

For a longer example, walk into `\f -> \x -> f (f x)`. Entering the first
lambda pushes `f` onto a scope stack. Entering the second pushes `x`:

```text
scope, nearest first:  [x, f]
index:                  0  1

f (f x)                 1 (1 0)
\f -> \x -> f (f x)     λ. λ. 1 (1 0)
```

Both occurrences of `f` have index 1. Application adds no binder, so entering
`(f x)` does not change either index. Renaming the parameters to `g` and `seed`
produces exactly the same indexed tree. Alpha-equivalence has become ordinary
structural equality [@pierce2002]. This is useful in our printer tests. A
parse–print–parse round trip should preserve binding even if names change.

An open term still needs a way to represent free variables. Our `Term` type
keeps them as `Free(name)`, separate from `Bound(index)`. Thus `\x -> x y`
becomes `Lam(App(Bound(0), Free("y")))`. A non-recursive `let` introduces a binder
only in its body. In `let x = x in x`, the right-hand `x` is free and the final
`x` is bound:

```text
Let(Free("x"), Bound(0))
```

Indices make renaming simple, but moving terms across binders still requires
care. Inside `\a -> (\x -> \b -> x) a`, the argument `a` has index 0 at its
original location. After reduction the result is `\a -> \b -> a`, where the
same occurrence needs index 1. Copying the old zero would accidentally select
`b`. An evaluator that substitutes directly into indexed terms needs _shifting_:
adjust references to surrounding binders when a term moves into or out of a
scope, while leaving its internally bound references alone.

Here we use indices for conversion and comparison. The normal-order reducer
still substitutes in the named tree, and the strict evaluator uses closures.
That keeps each example focused. Later Fun passes use unique names so their
dumps remain readable. Nameless terms are a useful tool for understanding
binding, not a requirement for every compiler IR.

### S, K, and I: Functions Without Binders

A _combinator_ is a lambda term with no free variables. It needs no bindings
from its surroundings. Three small combinators are enough to express the
functions of the pure untyped lambda calculus:

```text
I = \x -> x
K = \x -> \y -> x
S = \f -> \g -> \x -> f x (g x)
```

| Reduction rule        | Role                                                |
| --------------------- | --------------------------------------------------- |
| `I a → a`             | Pass the argument through.                          |
| `K a b → a`           | Keep the first argument.                            |
| `S f g a → f a (g a)` | Supply an argument to both sides of an application. |

These capital letters are mathematical names. The executable tests use
lowercase `s`, `k`, and `i` as ordinary local bindings. Applications associate
left, so `S f g a` means `((S f) g) a`. Each reduction rule needs all its displayed
arguments before it can fire.

`I` and `K` are easy to picture. `S` explains how an application can depend on
a shared input. Given `f`, `g`, and `a`, compute `f a` and `g a`, then apply the
first result to the second. The parentheses in `f a (g a)` matter.

Try choosing `K` for both functions:

```text
S K K a
→ K a (K a)        apply the S rule
→ a                apply the K rule
```

So `S K K` behaves as `I`. That's a beautiful little result. We can build
identity out of two operations that, at first glance, don't look much like
identity at all. S and K suffice, though keeping I around makes expressions
easier to read. Try the chapter's `s k k (i 42)` example and follow the reductions.

Why should these particular operations eliminate lambdas? For a body `t`, write
`[x]t` for a combinator expression that accepts `x` and produces `t`. Once inner
lambdas have been translated, there are three cases:

```text
[x]x       = I
[x]t       = K t                  when x is not free in t
[x](p q)   = S ([x]p) ([x]q)       otherwise
```

If the body is the parameter itself, return it. If the body ignores the
parameter, use `K` to discard it. Otherwise the body is an application, and `S`
passes the parameter to both translated children. This procedure is called
_bracket abstraction_. Work from the innermost lambda outward. Translating a
closed pure lambda term eventually leaves only combinators and applications.

For example, remove the binder from `\x -> f x`, treating `f` as a free name:

```text
[x](f x)
= S ([x]f) ([x]x)
= S (K f) I
```

Check the result by giving it an argument:

```text
S (K f) I a
→ (K f a) (I a)
→ f (I a)
→ f a
```

The occurrence of `x` has disappeared. The free `f` remains, as it should.
De Bruijn indices retain binders and encode their references as distances.
Bracket abstraction removes the binders and expresses their work as application.
This gives us a small example of compilation between two representations of the
same computation.

There is a catch, and it is the same evaluation-order question we started
with. Normal order reduces `K 42 omega` to `42` even if `omega` runs forever.
Our strict evaluator tries to compute `omega` before entering K's body, so it
never gets that far. These combinator equations describe rewriting in the pure
calculus. They don't let a strict compiler discard arbitrary computations.
We'll use SKI to understand binding and reduction, while Fun's back end will
use closures. Closures also spare us the large application trees that a simple
combinator translation can produce.

## Implementation

`ast.rs` adds lambdas, application, booleans, and conditionals. Its printer uses
precedence contexts and adds parentheses only where required to retain the tree.
The parser gives application higher precedence than arithmetic:

```text
{{#include ../../crates/ch03_lambda/src/parser.lalrpop:application}}
```

`names.rs` computes free variables and performs capture-avoiding substitution.
The helper below handles a binder. The recursive substitution handles each term
form. Fresh names avoid both free and bound names in the original body and the
replacement, preventing a new name from colliding with an inner lambda.

```rust
{{#include ../../crates/ch03_lambda/src/names.rs:binder}}
```

The same module converts a named term to indices. A stack holds names from the
outermost to the innermost scope. A `let` right-hand side is converted before its
binder is pushed. That binder belongs only to the body.

```rust
{{#include ../../crates/ch03_lambda/src/names.rs:debruijn}}
```

`normal.rs` implements one small step. In the application case it substitutes
without first evaluating the argument. If the function is not a lambda, it steps
that function first, then the argument when no earlier redex exists.

```rust
{{#include ../../crates/ch03_lambda/src/normal.rs:beta}}
```

`reduce` repeatedly calls `step` with a limit and a callback that displays each
term. This reducer can inspect open terms, including irreducible applications.
It is a term-rewriting demonstration rather than Fun's eventual execution engine.

`eval.rs` instead stores a lambda's environment in a closure:

```rust
{{#include ../../crates/ch03_lambda/src/eval.rs:value}}
```

The call-by-value application case makes its order explicit:

```rust
{{#include ../../crates/ch03_lambda/src/eval.rs:cbv-application}}
```

The body runs in the captured environment extended with the argument. Each
recursive visit consumes fuel. The CLI uses a larger host stack for these tree
walkers. Chapter 5 replaces host recursion with an explicit machine. Neither
fuel nor a larger stack is a substitute for that transformation.

## Running It

```text
$ cd crates/ch03_lambda
$ cargo run -q -- run tests/programs/church.fun
2
$ cargo run -q -- dump --pass=debruijn tests/programs/identity.fun
App(Lam(Bound(0)), Int(42))
$ cargo run -q -- repl
Lambda: :quit, :parse EXPR, :trace EXPR.
lambda> :trace (\x -> x) 42
(\x -> x) 42
42
lambda> :quit
```

Select normal order with `run --engine=normal`. Use `--fuel=N` when experimenting
with terms that might not finish. The two engines count different operations,
so the same fuel limit needn't stop them at the same point. Try a function that
ignores a failing argument to see why their answers can differ.

## Exercises

1. **Easy.** Convert `\x -> \y -> y x` to de Bruijn indices by hand.
2. **Medium.** Substitute the free name `y` for `x` in `\y -> \y' -> x y`. Check the result's free variables.
3. **Medium.** Translate `\x -> x x` by bracket abstraction, then apply the result to `I`. Compare its reduction with the lambda term.
4. **Medium.** Trace `K I a b` and explain which argument is discarded.
5. **Hard.** Construct a fixed-point combinator suitable for call-by-value and compare its trace with Y.

## Further Reading

Pierce treats substitution, reduction, and nameless terms [@pierce2002].
Felleisen and Friedman introduce the abstract-machine setting
that we return to in Chapter 5 [@felleisen1986].
