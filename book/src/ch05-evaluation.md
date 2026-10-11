# Chapter 5: Evaluation

> **New files**
>
> - `src/cek.rs`
> - `src/cbn.rs`
>
> **Changed files**
>
> - `src/eval.rs`
> - `src/types.rs`
> - `src/ast.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/names.rs`
> - `src/normal.rs`

## Motivation

A mathematical evaluation rule often describes a recursive computation. Writing
that rule as a recursive Rust function is convenient, but it also gives the
program's control stack to Rust. A tail-recursive loop can then exhaust the host
stack even though it has no pending work. A larger stack only postpones the
failure.

An abstract machine makes that pending work explicit. Its control component
says what to evaluate, its environment gives meanings to variables, and its
continuation records what remains. These are the C, E, and K of the CEK machine
[@felleisen1986]. A single Rust loop drives the machine, and entering a function
body does not call the evaluator recursively.

## The language, now

A recursive binding gives its type before its right-hand side. That right-hand
side must be syntactically a lambda. A value such as `let rec x : Int = x in x`
is rejected with `E0120`.

```fun
let rec sum : Int -> Int -> Int =
  \(acc : Int) -> \(n : Int) ->
    if n == 0 then acc else sum (acc + n) (n - 1)
in sum 0 100
```

The self name is available inside the function and its enclosing `let` body.
Its annotation is checked against the inferred right-hand-side type. `fix` is
also an explicit term form, binding tighter than application:

```fun
fix (\(self : Int -> Int) -> \(n : Int) ->
  if n == 0 then 1 else n * self (n - 1)) 6
```

Both examples are well typed, but only the sum is tail recursive. Multiplication
must wait for each recursive factorial call to return.

## Theory

A fixed point of a function satisfies the unfolding equation: `fix f` behaves
as `f (fix f)`. Its type rule requires the argument to return the same type
that it accepts [@pierce2002].

$$
\frac{\Gamma \vdash f : \tau \to \tau}
{\Gamma \vdash \mathsf{fix}\ f : \tau}\;\text{(Fix)}
$$

Adding `fix` permits well-typed nontermination. The expression
`fix (\(x : Int) -> x)` consumes every fuel budget. Types alone no longer
provide normalization. Recursive `let` lowers to a fixed point, but its
function-only restriction makes initialization predictable. This is the
restriction Fun will retain for recursive binding groups.

Call-by-value evaluates a function and its argument before entering its body.
Call-by-name substitutes an unevaluated argument each time it is used and stops
at a lambda. Call-by-need additionally shares the argument's result, requiring
a suspended computation and a memoized result. Our small comparison interpreter
implements name, without memoization: its substitution code makes the absence
of sharing explicit. Chapter 3's full normal-order reducer also enters lambda
bodies, unlike this weak call-by-name evaluator.

Fun chooses call-by-value to make order and allocation easier to follow through
its intermediate languages. Constructors will also be strict. A lambda remains
a value, so the language can represent an action without performing it yet.
Chapter 15 uses this property for `IO`. Laziness would require another runtime
representation and another account of effects and space usage, beyond this
book's chosen compiler.

## Implementation

The parser checks the recursive-binding restriction and lowers an accepted
binding through this constructor:

```rust
{{#include ../../crates/ch05_eval/src/ast.rs:recursive}}
```

`Value::Recursive` stores the function used to construct a fixed point. Reading
that recursive variable applies the stored function to its recursive reference.
For a recursive function this rebuilds the closure in its original environment.
It does not form a Rust ownership cycle. Entering `fix` evaluates its defining
function immediately, so errors in that initialization are still strict.
This is a teaching implementation of explicit recursion, not a general lazy
binding mechanism.

`cek.rs` separates control from continuation frames. The frame variants are the
places where the tree evaluator would otherwise wait for a recursive call.

```rust
{{#include ../../crates/ch05_eval/src/cek.rs:machine}}
```

An application first pushes an `Argument` frame while evaluating its function.
That frame then becomes an `Apply` frame while evaluating the argument. Once
both values are available, the machine enters the body in the closure's captured
environment extended with the parameter. The application frames have already
been popped:

```rust
{{#include ../../crates/ch05_eval/src/cek.rs:tail-transfer}}
```

There is no return frame for a tail call. A surrounding addition does retain a
frame, because it has work left to do. Thus both kinds of recursion use constant
Rust stack, while only tail recursion has bounded continuation storage. The
million-call test runs on a 256 KiB host thread stack and checks that its maximum
continuation depth equals that of a hundred-call run. A separate non-tail sum
test confirms that its explicit stack grows.

The complete transition loop follows the expression forms and pops one frame
when a value returns. `max_continuations` is measured before every transition,
and a fuel counter makes divergent examples safe to inspect. Fuel counts
machine transitions, so its numerical value differs from the tree evaluator's
count of expression visits.

The tiny call-by-name evaluator uses substitution instead of an environment
for its application and `let` cases:

```rust
{{#include ../../crates/ch05_eval/src/cbn.rs:call-by-name}}
```

A call-by-need variant would replace this substitution with a shared cell whose
first demand computes and saves the argument. We leave that implementation for
future work. The production path here remains the strict machine, and the
comparison interpreter is selected explicitly.

## Running it

```text
$ cd crates/ch05_eval
$ cargo run -q -- run tests/programs/sum-tail.fun
5050
$ cargo run -q -- run --engine=core tests/programs/factorial.fun
720
$ cargo run -q -- dump --pass=types tests/programs/fix-factorial.fun
Int
$ cargo run -q -- run --engine=cbn tests/programs/strict-argument.fun
1
$ cargo test -q a_million_tail_calls_use_bounded_continuations
```

The default engine is `cek`. `core` selects the recursive closure evaluator,
`cbn` selects the comparison interpreter, and `normal` retains the earlier
full reducer. `--fuel=N` bounds execution. Use a modest fuel budget for the
tree interpreters: only CEK removes their dependence on the host stack.
All 35 inherited fixtures are unchanged. The golden runner compares both strict
engines on the 50-program corpus. Non-strict execution deliberately has different
results for the error-order examples.

## Exercises

1. **Easy.** Draw the continuation frames for `1 + 2 * 3` as each operand returns.
2. **Medium.** Change the tail sum into a non-tail sum and measure both maximum continuation depths.
3. **Medium.** Explain why a closure must capture its definition environment rather than its caller's environment.
4. **Hard.** Give an expression for which call-by-name repeats substantial work that call-by-value computes once. Explain what sharing would change.

## Further reading

Felleisen and Friedman develop the abstract-machine account [@felleisen1986].
Pierce relates evaluation strategies, typing, and fixed points [@pierce2002].
Appel develops the related technique of making continuations explicit during
compilation [@appel1992]. Our ANF compiler returns to that connection later.
