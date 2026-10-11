# Chapter 6: Hindley–Milner Inference

> **New files**
>
> - `src/algorithm_w.rs`
> - `src/union_find.rs`
>
> **Changed files**
>
> - `src/types.rs`
> - `src/ast.rs`
> - `src/cbn.rs`
> - `src/cek.rs`
> - `src/eval.rs`
> - `src/lexer.rs`
> - `src/lib.rs`
> - `src/main.rs`
> - `src/names.rs`
> - `src/normal.rs`

## Motivation

The identity function does not inspect its argument. Its behavior works for an
integer, a boolean, or another function, yet our simple type checker required
one parameter annotation. A type scheme describes that flexibility: each use
may choose a fresh type for the quantified parameter. Hindley–Milner inference
finds a principal scheme from the program itself [@damas1982].

We implement inference twice. Algorithm W keeps substitutions as explicit maps,
following the classical presentation [@damas1982]. The second algorithm
stores mutable solutions in a vector and uses levels to track which variables
can be generalized. Both solve the same equations. Keeping them independent
lets their results check one another before we choose the latter for Fun.

## The language, now

Poly has unannotated lambdas, application, polymorphic `let`, function-only
`let rec`, integers, booleans, conditionals, and arithmetic. The explicit `fix`
form remains available from the evaluation chapter for comparison exercises.

```fun
let id = \x -> x in
let first = id 1 in
id True
```

The two occurrences of `id` instantiate its scheme independently. Lambda
parameters do not behave this way: `\f -> let first = f 1 in f True` is rejected,
because `f` has one type throughout that lambda body.

Annotations from Chapter 5 are removed, including recursive-binding annotations.
Some inherited error examples consequently become valid: an identity no longer
has an imposed `Int` parameter type. Every changed source or expectation is
listed in `BREAKING.md`. This is a new standalone mini-language, not a silent
change to an earlier chapter.

## Theory

### How to read a typing rule

A typing rule is a small contract for one piece of syntax. Read the conclusion
first to find the expression it describes, then read the premises to see what
must be established about its parts. Pierce uses this style to make a type
system precise without tying it to one implementation [@pierce2002].

$$
\Gamma \vdash e : \tau
$$

Read this as “under the assumptions in gamma, expression e has type tau.” The
symbols keep repeated sentences short:

| Notation                              | Meaning                                                                  |
| ------------------------------------- | ------------------------------------------------------------------------ |
| $e$                                   | A program expression, such as `x`, `42`, or `f x`.                       |
| $\tau$, $\alpha$, $\beta$             | A type, or a type variable standing for a type.                          |
| $\sigma$                              | A type scheme, possibly quantifying type variables.                      |
| $\Gamma$                              | The typing environment: names and the types or schemes we know for them. |
| $\Gamma, x : \tau$                    | Extend the environment with a local assumption about `x`.                |
| $\vdash$                              | “Under these assumptions, we can establish …”                            |
| $\alpha \to \beta$                    | A function accepting an $\alpha$ and returning a $\beta$.                |
| $\forall \alpha.\; \alpha \to \alpha$ | A scheme whose user may choose the type replacing $\alpha$.              |

The colon asserts a type. It does not evaluate the expression. Gamma contains
static assumptions, not runtime values. In this chapter it maps let-bound names
to schemes and lambda parameters to monotypes, represented as schemes with no
quantifiers. Greek variables in the rules are mathematical placeholders. The
inferencer's mutable metavariables are an implementation technique for finding
which types satisfy the rules.

The horizontal bar means “if everything above holds, then the statement below
holds.” It is not division. A rule with an empty top has no premises: integer
literals have type `Int` without consulting a subexpression.

$$
\frac{}{\Gamma \vdash 42 : \mathsf{Int}}\;\text{(Int)}
$$

For application, two facts must agree on the same input type:

$$
\frac{\Gamma \vdash e_1 : \alpha \to \beta \quad
\Gamma \vdash e_2 : \alpha}
{\Gamma \vdash e_1\ e_2 : \beta}\;\text{(App)}
$$

If `e1` accepts an integer and returns a boolean, `e2` must be an integer and
the call returns a boolean. The repeated $\alpha$ expresses that agreement.
During inference we discover it by unification. The rule describes a valid
relationship. It does not prescribe an order in which to solve the premises.

### Build a small derivation

Consider `\x -> x + 1`. Give `x` a provisional type. The addition rule requires
both operands to be integers, which fixes that provisional type to `Int`.
Writing $\Delta = \Gamma, x : \mathsf{Int}$ keeps the resulting tree compact:

$$
\frac{
  \frac{x : \mathsf{Int} \in \Delta}{\Delta \vdash x : \mathsf{Int}}\;\text{(Var)}
  \quad
  \frac{}{\Delta \vdash 1 : \mathsf{Int}}\;\text{(Int)}
}{\Delta \vdash x + 1 : \mathsf{Int}}\;\text{(Add)}
$$

The two leaves justify Add by lookup and the literal rule. The lambda rule
then moves the assumption about the parameter into a function type:

$$
\frac{\Gamma, x : \alpha \vdash e : \beta}
{\Gamma \vdash \lambda x.\;e : \alpha \to \beta}\;\text{(Lam)}
$$

Here $\lambda x.\;e$ means `\x -> e`. Lam gives our function type `Int -> Int`
and removes the body's local assumption about `x`. Replacing `1` with `True`
breaks an Add premise: the attempted derivation cannot be completed.

### From monotypes to schemes

A monotype may contain variables, and a scheme quantifies some of them.
Instantiation replaces quantified variables with fresh metavariables.
Generalization quantifies the variables of a binding's type that are not free
in its environment. A lambda parameter is stored as a scheme with no quantified
variables [@damas1982].

$$
\frac{\Gamma \vdash e_1 : \tau \quad
\sigma = \mathsf{gen}(\Gamma, \tau) \quad
\Gamma, x : \sigma \vdash e_2 : \beta}
{\Gamma \vdash \mathsf{let}\ x = e_1\ \mathsf{in}\ e_2 : \beta}
\;\text{(Let)}
$$

Read Let in three steps: infer the binding's right-hand side, generalize its
type, and check the body with the resulting scheme in scope. The equation for
$\sigma$ is a side condition defining that scheme, rather than another program
to evaluate. A variable is _free_ in a type or environment when it is not bound
by a scheme's quantifier. Only variables absent from the environment's free
variables may be generalized.

For `let id = \x -> x in ...`, the right-hand side has type
$\alpha \to \alpha$. If $\alpha$ is independent of the surrounding environment,
we bind `id` to $\forall \alpha.\;\alpha \to \alpha$. Each lookup instantiates
this scheme afresh: `id 1` chooses `Int`. `id True` chooses `Bool`. By contrast,
the parameter `f` in `\f -> ...` receives one monotype from Lam. All its uses
must satisfy that same type. Quantification belongs to the let rule, not to
every name that happens to denote a function.

Unification solves equality constraints between types. Two arrows unify by
unifying their domains, applying what that learned, and unifying their codomains.
A variable can be solved by a type only if it does not occur inside that type.
Without this occurs check, self-application would require an infinite type.
It is reported as `E0202`. Incompatible base types produce `E0201`.

A recursive function has one provisional monotype while its body is checked.
Only after that check may the surrounding `let` generalize it. Our lowering to
`fix` expresses this rule through the ordinary monomorphic self parameter.
There is no polymorphic recursion here. The environment-based generalization
rule and its efficient level-based form are developed in the cited accounts
[@kiselyov2013; @remy1992].

Poly has no mutable state, so a `let` right-hand side need not be a syntactic
value to generalize. For example, applying an identity to another identity still
produces a polymorphic binding. This unrestricted rule will also apply to Fun.

## Implementation

The algorithms share immutable output types and schemes, but no unifier or
inference traversal. Variable identities are canonicalized by first occurrence
only after inference. That makes their principal types comparable up to renaming.

```rust
{{#include ../../crates/ch06_poly/src/types.rs:types}}
```

### Algorithm W

`algorithm_w.rs` separates substitution, instantiation, generalization, and
inference. The core rules extend to Poly's arithmetic, conditional, and
fixed-point forms, with source spans retained for diagnostics.

A substitution maps variables to types. Composition means applying the earlier
substitution first, then rewriting its result with the newer one. Applying a
substitution to a scheme must protect the scheme's quantified variables.

```rust
{{#include ../../crates/ch06_poly/src/algorithm_w.rs:substitution}}
```

Application threads information learned from the function into the environment
used to infer its argument. It then unifies the function's type with an arrow
from the argument's type to a fresh result.

```rust
{{#include ../../crates/ch06_poly/src/algorithm_w.rs:application}}
```

The `let` case applies the right-hand side's substitution to the environment
before generalizing. Generalizing against the old environment would allow an
already constrained variable to escape.

```rust
{{#include ../../crates/ch06_poly/src/algorithm_w.rs:let}}
```

### Mutable solutions and levels

`union_find.rs` replaces the explicit substitution map with metavariable cells.
A cell records its creation level and an optional solution. Following a solved
variable compresses its path, so later visits can reach the resolved type sooner.

```rust
{{#include ../../crates/ch06_poly/src/union_find.rs:store}}
```

Inferring a `let` right-hand side enters a deeper level. Before installing a
solution for an older metavariable, the occurs-check traversal lowers every
metavariable reachable from that solution to the older level. These variables
now belong to the outer scope, even if they were allocated inside the `let`.

```rust
{{#include ../../crates/ch06_poly/src/union_find.rs:levels}}
```

After returning to the enclosing level, generalization selects only unsolved
variables whose levels are still deeper. It does not scan the environment.
Every occurrence of a scheme replaces its quantified variables with fresh cells.
Unquantified variables retain their identities and therefore their constraints.

```rust
{{#include ../../crates/ch06_poly/src/union_find.rs:let}}
```

Tests target the distinction directly. In `\x -> let y = \z -> x z in ...`,
`y` cannot acquire a type independent of `x`. Using it at both `Int` and `Bool`
must fail. Other tests cover unrestricted generalization, recursive bindings,
occurs checks, and principal types for composition and Church numerals. The
algorithms agree on the whole corpus and on 1,000 deterministic generated terms,
including ill-typed terms.

## Running it

```text
$ cd crates/ch06_poly
$ cargo run -q -- run tests/programs/poly-id.fun
True
$ cargo run -q -- dump --pass=types tests/programs/missing-annotation.fun
a -> a
$ cargo run -q -- dump --pass=types-w tests/programs/missing-annotation.fun
a -> a
$ cargo run -q -- repl
Poly: :quit, :type EXPR, :parse EXPR, :trace EXPR.
poly> :type \f -> \x -> f x
(a -> b) -> a -> b
poly> :quit
```

`types` and the normal checking path use the level-based inferencer. `types-w`
selects Algorithm W's independent result. Free variables in the displayed
principal type are implicitly quantified. Evaluation still uses the strict CEK
machine by default. Changing inference does not change its runtime values.

## Exercises

1. **Easy.** Infer the type of `\x -> \y -> x` by writing down fresh variables first.
2. **Medium.** Trace the substitutions for `let id = \x -> x in id id`.
3. **Medium.** Explain why `\f -> let a = f 1 in f True` fails while the opening `id` example succeeds.
4. **Hard.** Draw the metavariable cells and levels for an inner function that captures an outer lambda parameter. Identify the unification that lowers a level.

## Further reading

Damas and Milner establish principal type schemes [@damas1982]. Kiselyov explains
levels through the OCaml implementation [@kiselyov2013], building on Rémy's work
[@remy1992]. Pierce provides a broader introduction to typing judgments and
derivations [@pierce2002].

See [my alternative writeup about just type systems](https://sdiehl.github.io/typechecker-zoo/)
for another route through these ideas.
