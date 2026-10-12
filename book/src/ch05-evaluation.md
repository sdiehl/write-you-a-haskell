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

A recursive evaluator is a lovely way to explain evaluation. Each Rust call
follows one more piece of the program. Unfortunately, each call can also use
another piece of Rust's stack. Our little language has inherited a limit we
never intended to give it. A bigger stack buys us time, but the problem is
still there.

We'll fix this by writing down what the evaluator needs to do next. Instead
of leaving that information in Rust call frames, we'll store it in a data
structure and drive the whole evaluator with a loop. This is the idea behind
an abstract machine.

The machine we'll use is called CEK. The C tells us what we're evaluating, the
E records the current variable bindings, and the K records what comes next
[@felleisen1986]. The name is more intimidating than the idea. We're turning
the evaluator's implicit bookkeeping into something we can inspect.

## The Language, Now

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

A fixed point of a function satisfies an unfolding equation. `fix f` behaves
as `f (fix f)`. Its type rule requires the argument to return the same type
that it accepts [@pierce2002].

$$
\frac{\Gamma \vdash f : \tau \to \tau}
{\Gamma \vdash \mathsf{fix}\ f : \tau}\;\text{(Fix)}
$$

With `fix`, a well-typed program can run forever. Try
`fix (\(x : Int) -> x)` and watch it use up any fuel budget you give it.
The type checker hasn't failed us. It never promised this recursive program
would terminate. Recursive `let` uses the same fixed-point idea, but requires
a function on the right-hand side so we know how to initialize it. Fun will
keep that restriction.

Call-by-value evaluates a function and its argument before entering its body.
Call-by-name substitutes an unevaluated argument each time it is used and stops
at a lambda. Call-by-need additionally shares the argument's result, requiring
a suspended computation and a memoized result. Our small comparison interpreter
implements call-by-name without memoization. Its substitution code makes the absence
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

Look at the sum again. Its recursive call already produces the answer, so
there's nothing to remember for the caller. Factorial still has a multiplication
to finish. That pending work needs a frame.

Both versions now avoid growing Rust's call stack. Only the tail-recursive
version avoids growing our explicit continuation stack as well. We check this
by comparing the maximum continuation depth of a short run with a million-call
run. Finishing on one generously sized machine wouldn't tell us nearly as much.

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

## Running It

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
tree interpreters. CEK is the one that removes their dependence on the host stack.
The two strict engines should agree on both answers and failures. The
call-by-name comparison deliberately differs when an unused argument would
fail. Try it before reading the implementation and predict which error, if
any, you'll see.

## Exercises

1. **Easy.** Draw the continuation frames for `1 + 2 * 3` as each operand returns.
2. **Medium.** Change the tail sum into a non-tail sum and measure both maximum continuation depths.
3. **Medium.** Explain why a closure must capture its definition environment rather than its caller's environment.
4. **Hard.** Give an expression for which call-by-name repeats substantial work that call-by-value computes once. Explain what sharing would change.

## Further Reading

Felleisen and Friedman develop the abstract-machine account [@felleisen1986].
Pierce relates evaluation strategies, typing, and fixed points [@pierce2002].
Appel develops the related technique of making continuations explicit during
compilation [@appel1992]. Our ANF compiler returns to that connection later.
