# Chapter 12: Records

> **New files**
>
> None.
>
> **Changed files**
>
> - `src/typecheck.rs`
> - `src/data.rs`
> - `src/lexer.rs`
> - `src/elaborate.rs`
> - `src/ast.rs`
> - `src/core.rs`
> - `src/desugar.rs`
> - `src/eval.rs`
> - `src/fixity.rs`
> - `src/format.rs`
> - `src/main.rs`
> - `src/parse.rs`
> - `src/rename.rs`
> - `src/renamed.rs`

## Motivation

Positional constructors become hard to read when several fields have the same
type. A point represented by two integers is small enough to remember. A larger
configuration or syntax node is not. Field names make construction and access
state their intent without changing the underlying algebraic datatype.

Traditional Haskell record selectors expose two problems for this design. A
selector may be partial when only some constructors contain its field, and the
selector's generated function occupies the ordinary value namespace. Fun avoids
both: a record must have exactly one constructor, and fields are accessed through
dot syntax rather than generated functions. Different record types may therefore
reuse a field name [@haskell2010; @recorddot].

## The language, now

```fun
data Point = Point { x :: Int, y :: Int }

move p = p { x = p.x + 1 }
main = move (Point { y = 4, x = 3 })
```

Construction must supply every field exactly once. Its expressions run in written
order, even when that differs from declaration order. Positional construction and
patterns remain available. A record pattern may omit fields, which become
wildcards: `Point { x = n }` binds only the first coordinate.

Projection and update bind more tightly than application. Thus `f p.x` means
`f (p.x)`, and `f p { x = 3 }` means `f (p { x = 3 })`. The section `(.x)` is
shorthand for a lambda. Chained access such as `shape.origin.x` projects twice.
An update preserves the receiver's complete type, including its type arguments.

```fun
data Label = Label { x :: String }

coordinate :: Point -> Int
coordinate p = p.x
```

The signature identifies which `x` is meant. The parenthesized expression
annotation `(p :: Point).x` works too. If only one record declares a field, the
checker can infer its receiver type without an annotation.

## Theory

These are nominal records: the declaration determines their identity. Two types
with identical fields remain different types. This differs from a structural or
row-polymorphic system, where a function may accept any record containing a named
field [@pierce2002].

Projection introduces a wanted constraint linking the receiver and result types:

$$
\frac{\Gamma \vdash e : \tau \quad \mathsf{Has}(\tau,x,\alpha)}{\Gamma \vdash e.x : \alpha}\;\text{(Projection)}
$$

`Has` is an internal checking obligation. It is never generalized into a scheme.
For a unique field name, the checker immediately unifies the receiver with that
record type, using fresh parameters. Otherwise it waits for the receiver's nominal
head to become known. Before generalizing a binding group it repeats resolution
to a fixpoint. Any remaining field wanted produces `E0245` and an annotation hint.

GHC's record-dot proposal is useful prior art for the surface syntax. Its
`HasField` mechanism supports a different form of overloaded access. Fun's field
solver always chooses a concrete nominal record before elaboration [@recorddot].

## Implementation

### Classifying the dot

Whitespace is significant for this token. `f.g` means projection, while `f . g`
uses the ordinary composition operator. The post-pass compares original token
spans before trivia capture and layout can obscure adjacency:

```rust
{{#include ../../crates/ch12_records/src/lexer.rs:projection-dots}}
```

The token preceding a projection must be an identifier, literal, or closing
delimiter. A dot immediately following `(` and preceding a variable identifier is
a projection-section token. Other dots remain operators. In particular, the
normative rule also leaves `x .y` as an ordinary operator occurrence.

`E0004` diagnoses malformed recognized sections such as `(.x y)`. The formatter
keeps dot and label tokens adjacent and moves attached comments outside that
sequence. Both decisions are tested because a whitespace-only rewrite could
otherwise change the program's meaning.

### Keeping field metadata nominal

Each record constructor retains field labels alongside its field types. Datatype
validation rejects a record mixed with other constructors (`E0240`) and repeated
declaration labels (`E0104`). Named construction checks missing, duplicate, and
unknown fields with `E0241`, `E0242`, and `E0243` respectively.

Record patterns are converted to positional constructor patterns during renaming.
The renamer looks up each label's slot and fills every omitted slot with a wildcard.
Consequently the pattern checker and Core lint need no new record-pattern rule.
No field is added to the value environment.

### Solving field obligations

A wanted records the receiver type, label, fresh field type, source site, and
inference level. Sites distinguish individual fields within an update:

```rust
{{#include ../../crates/ch12_records/src/typecheck.rs:field-wanted}}
```

The projection inference branch creates that obligation and returns its fresh
result type. Successful resolution later unifies the result with the instantiated
field declaration:

```rust
{{#include ../../crates/ch12_records/src/typecheck.rs:field-wanted-generation}}
```

`solve_field` either identifies a constructor and field slot, reports a type with
no such field (`E0244`), or leaves an unknown receiver unresolved. The surrounding
loop repeats because one resolved field may identify the receiver of another:

```rust
{{#include ../../crates/ch12_records/src/typecheck.rs:field-solver}}
```

Signature-directed checking supplies lambda parameter types before their bodies
are visited. This matters when a local binding accesses a shared field: the
outer signature must be available before that local group is generalized. The
existing rigid signature check still rejects an annotation that claims too much
polymorphism.

### Elaborating strict construction and access

Field order in source is independent of field order in memory. Construction
therefore introduces a strict let for each supplied expression, then builds a
constructor using those variables in declaration order:

```rust
{{#include ../../crates/ch12_records/src/elaborate.rs:record-construction}}
```

Projection evaluates its receiver once, matches its sole constructor, and returns
the selected field binder. Update uses the same match, evaluates replacement
expressions in source order, and rebuilds the constructor with unchanged fields
in their original slots. The result type equals the receiver type, so an update
cannot turn `Box Int` into `Box Bool`.

Core sees only constructs introduced in [Chapter 11](ch11-datatypes.md). Independent lint checks their
types, and the interpreter executes ordinary matches and constructors. Its value
printer consults the constructor's labels to print `Point {x = 3, y = 4}` in
declaration order. Later native projection will become a field load.

## Running it

```text
$ cd crates/ch12_records
$ cargo run -q -- run tests/programs/records/nested.fun --lint
(3,Outer {inner = P {x = 10, y = 4}})
$ cargo run -q -- dump --pass=types tests/programs/records/shared-signature.fun
main : Int
$ cargo test --test records
```

The corpus contains 209 programs: all 171 inherited programs are unchanged, and
38 new cases cover records and their diagnostics. Ten new snapshots show the Core
lowering. Targeted tests check dot tokenization, comment-preserving formatting,
and failure order when both the receiver and replacement fields can fail.

## Exercises

1. **Introductory:** define a polymorphic `Box a` record and infer the type of
   its projection section.
2. **Intermediate:** add another record with the same field label, observe the
   ambiguity error, and resolve it with a signature.
3. **Intermediate:** explain why another uniquely named field can disambiguate
   an earlier unresolved access in the same binding group.
4. **Advanced:** write a construction whose source field order differs from its
   declaration order and whose first failure reveals the evaluation order.
5. **Advanced:** identify the parts of inference and generalization that would
   need to change to support row-polymorphic access. Do not add that feature here.

## Further reading

The Haskell Report describes traditional record construction, update, and selector
functions [@haskell2010]. GHC proposal 282 motivates record-dot syntax and its
relationship to overloaded field access [@recorddot]. Pierce develops structural
record typing, a useful comparison with Fun's nominal choice [@pierce2002].
