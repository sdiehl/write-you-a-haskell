# Appendix B: Typing Rules

These rules give a declarative account of Fun's source type system. They collect
the judgments used throughout the book, including patterns, binding groups,
classes, records, and IO. They describe which programs are well typed. The
implementation finds those derivations using unification, levels, and constraint
solving. Core has its own explicitly polymorphic typing judgment, introduced in
[Chapter 10](ch10-typed-core.md).

## How to Read a Rule

Read a fraction from bottom to top as a question: to establish the conclusion,
establish each premise above the line. Read it from top to bottom as a proof:
these premises justify that conclusion. A rule with no premises is an axiom.
The name in parentheses identifies the step, not an extra condition [@pierce2002].

Our main judgment is $P;\Gamma\vdash e:\tau$. It says that expression $e$ has
type $\tau$ when the variables in $\Gamma$ have their listed schemes and the
class predicates in $P$ hold. The semicolon separates environments. It has no
meaning in Fun source. For example:

$$
\frac{x:\mathsf{Int}\in\Gamma}
     {P;\Gamma\vdash x:\mathsf{Int}}\;\text{(Var-Int)}
$$

The typing environment is a finite map. Extending it with $x:\tau$ shadows an
outer binding of $x$. A bar means a sequence, so $\overline{\alpha}$ stands for
zero or more type variables. Rules with a subscript $i$ apply to every member of
the indicated sequence. Side conditions written below a rule are part of it.

| Notation         | Meaning                                                  |
| ---------------- | -------------------------------------------------------- |
| $\tau$           | A monotype: variable, constructor, application, or arrow |
| $\sigma$         | A scheme $\forall\overline{\alpha}.\;Q\Rightarrow\tau$   |
| $P,Q$            | Sets of class predicates, such as $\mathsf{Eq}\;\alpha$  |
| $\Gamma$         | Term variables and their schemes                         |
| $\Delta$         | Type variables and constructors with their kinds         |
| $S\tau$          | Capture-avoiding substitution in a type                  |
| $\mathsf{fv}(X)$ | Free type variables of $X$                               |
| $P\vdash Q$      | The givens $P$ entail every predicate in $Q$             |
| $\Gamma_p$       | Monomorphic variables introduced by a pattern            |

Types are identified after expansion of fully applied synonyms. Schemes are
identified up to renaming of bound variables. The global signature records data
constructors, record fields, classes, and instances. All rules below assume that
signature has passed the declaration checks.

## Kinds and Type Formation

Kinds are $* $ and $\kappa_1\to\kappa_2$. The star means a type whose expressions
can have values. Kind arrows classify type constructors, not term functions.

$$
\frac{\alpha:\kappa\in\Delta}{\Delta\vdash\alpha:\kappa}\;\text{(K-Var)}
\quad
\frac{T:\kappa\in\Delta}{\Delta\vdash T:\kappa}\;\text{(K-Con)}
$$

$$
\frac{\Delta\vdash\tau_1:\kappa_1\to\kappa_2
      \quad\Delta\vdash\tau_2:\kappa_1}
     {\Delta\vdash\tau_1\;\tau_2:\kappa_2}\;\text{(K-App)}
$$

$$
\frac{\Delta\vdash\tau_1:*\quad\Delta\vdash\tau_2:*}
     {\Delta\vdash\tau_1\to\tau_2:*}\;\text{(K-Arrow)}
$$

Every term type and constructor field has kind $*$. In a predicate $C\;\tau$,
$\tau$ must have the kind of the class parameter. Thus `Functor` accepts a
constructor of kind $*\to *$, while `Eq` accepts a type of kind $*$. Kinds are
inferred jointly within each type-declaration component. Remaining unconstrained
kind variables become $*$. Synonym cycles and partial synonym applications are
errors, even if later unification could otherwise hide them.

## Variables, Literals, Functions

$$
\frac{x:\forall\overline{\alpha}.\;Q\Rightarrow\tau\in\Gamma
      \quad P\vdash SQ}
     {P;\Gamma\vdash x:S\tau}\;\text{(Var)}
$$

Here $S$ replaces the quantified variables with monotypes of matching kinds.
Constructors use the same rule with their declared schemes. Literals have fixed
types, without a class constraint:

$$
\frac{}{P;\Gamma\vdash n:\mathsf{Int}}\;\text{(Int)}
\quad
\frac{}{P;\Gamma\vdash d:\mathsf{Double}}\;\text{(Double)}
$$

$$
\frac{}{P;\Gamma\vdash c:\mathsf{Char}}\;\text{(Char)}
\quad
\frac{}{P;\Gamma\vdash s:\mathsf{String}}\;\text{(String)}
$$

$$
\frac{P;\Gamma,x:\tau_1\vdash e:\tau_2}
     {P;\Gamma\vdash\lambda x.e:\tau_1\to\tau_2}\;\text{(Abs)}
$$

$$
\frac{P;\Gamma\vdash e_1:\tau_1\to\tau_2
      \quad P;\Gamma\vdash e_2:\tau_1}
     {P;\Gamma\vdash e_1\;e_2:\tau_2}\;\text{(App)}
$$

A lambda parameter is monomorphic throughout its body. Instantiating `id` twice
may choose different types. Using a lambda-bound `f` twice must respect the same
type. Pattern parameters use the pattern rules below before applying (Abs).

## Let, Generalization, and Signatures

$$
\frac{P,Q;\Gamma\vdash e_1:\tau_1
      \quad P;\Gamma,x:\sigma\vdash e_2:\tau_2}
     {P;\Gamma\vdash\mathsf{let}\;x=e_1\;\mathsf{in}\;e_2:\tau_2}
     \;\text{(Let)}
$$

Here $\sigma=\forall\overline{\alpha}.\;Q\Rightarrow\tau_1$.
The generalized variables are free in $\tau_1$ or $Q$, but not in $\Gamma$ or
$P$. Every variable mentioned by $Q$ must also occur in $\tau_1$.
The solver first reduces and simplifies $Q$, discharges concrete predicates,
and resolves every field constraint. Predicates on variables outside this
binding's generalization scope remain in $P$. A residual predicate with no
possible generalization or instance is an error. There is no defaulting and no
value restriction.

For an unannotated recursive component, assign a fresh monotype $\tau_i$ to each
$x_i$, then check all bodies under those same assumptions:

$$
\frac{P,Q;\Gamma,\overline{x:\tau}\vdash e_i:\tau_i
      \quad P;\Gamma,\overline{x:\sigma}\vdash e:\tau}
     {P;\Gamma\vdash\mathsf{let}\;\overline{x=e_i}\;\mathsf{in}\;e:\tau}
     \;\text{(Let-Rec)}
$$

Generalize the solved types together relative to the outer environment, using
the same constraint and ambiguity conditions as (Let). All recursive right-hand
sides must be syntactic functions after desugaring. Recursion within an
unannotated component is monomorphic.

An annotation is a promise checked with fresh rigid variables:

$$
\frac{P,SQ;\Gamma\vdash e:S\tau}
     {P;\Gamma\vdash e:
        \forall\overline{\alpha}.\;Q\Rightarrow\tau}
     \;\text{(Signature)}
$$

Here $S$ replaces each quantified variable with a fresh skolem of the same kind.
These skolems cannot be unified with arbitrary types or escape into the outer
environment. The body's required predicates must follow from $P,SQ$ and the
instance environment. An annotated binding is available recursively at its
specified scheme, which permits polymorphic recursion. Dependency analysis
removes edges to annotated bindings before forming unannotated components.
Expression annotations are checked by this rule, then instantiated for use as
expressions. They do not introduce first-class polymorphic values.

## Patterns, Guards, and Cases

The judgment $\vdash p:\tau\Rightarrow\Gamma_p$ says that a pattern matches a
value of type $\tau$ and introduces the variables in $\Gamma_p$.

$$
\frac{}{\vdash x:\tau\Rightarrow x:\tau}\;\text{(P-Var)}
\quad
\frac{}{\vdash\_:\tau\Rightarrow\Gamma_0}\;\text{(P-Wild)}
$$

$$
\frac{\vdash p:\tau\Rightarrow\Gamma_p}
     {\vdash x@p:\tau\Rightarrow\Gamma_p,x:\tau}\;\text{(P-As)}
$$

$$
\frac{K:\forall\overline{\alpha}.\;
          \tau_1\to ... \to\tau_n\to T\;\overline{\alpha}
      \quad\vdash p_i:S\tau_i\Rightarrow\Gamma_i}
     {\vdash K\;\overline{p}:T\;S\overline{\alpha}
          \Rightarrow\overline{\Gamma_i}}\;\text{(P-Con)}
$$

Here $\Gamma_0$ is the empty environment. The pattern environments must have disjoint domains. Literal patterns have the
literal's fixed type and introduce no bindings. A record pattern is its
constructor pattern in declaration order, with wildcards for omitted fields.
List, tuple, and unit patterns use their ordinary constructors.

$$
\frac{P;\Gamma\vdash e:\tau_s
      \quad\vdash p_i:\tau_s\Rightarrow\Gamma_i
      \quad P;\Gamma,\Gamma_i\vdash e_i:\tau}
     {P;\Gamma\vdash\mathsf{case}\;e\;\mathsf{of}\;
          \overline{p_i\to e_i}:\tau}\;\text{(Case)}
$$

$$
\frac{P;\Gamma,\Gamma_p\vdash g_j:\mathsf{Bool}
      \quad P;\Gamma,\Gamma_p\vdash e_j:\tau}
     {P;\Gamma,\Gamma_p\vdash\overline{g_j\to e_j}:\tau}
     \;\text{(Guards)}
$$

Every guarded branch has the case's result type. A row's `where` bindings are
checked by the binding-group rules in the pattern environment, and scope over
both its guards and results. Function equations use the same row judgment after
introducing one argument per pattern column. All equations agree on arity.
Nonexhaustive and unreachable rows produce warnings, not additional typing rules.
Failure to match remains a possible runtime error at any result type.

A nonrecursive pattern binding first checks its right-hand side and pattern at
the same type. It then generalizes the introduced binders relative to the outer
environment, with the conditions of (Let). The pattern's binders are not in scope
on that binding's right-hand side.

## Nominal Records

Write $\mathsf{field}(T,l)=\tau_l$ for the field type in a record declaration
parameterized by $\overline{\alpha}$, and let $S$ substitute its actual type
arguments. Each rule requires the record constructor to have been identified.

$$
\frac{P;\Gamma\vdash e_i:S\tau_{l_i}}
     {P;\Gamma\vdash K\;\{\overline{l_i=e_i}\}:
        T\;S\overline{\alpha}}\;\text{(Record)}
$$

Every declared field occurs exactly once, with no unknown label. The source
order determines evaluation order even though field slots use declaration order.

$$
\frac{P;\Gamma\vdash e:T\;S\overline{\alpha}
      \quad\mathsf{field}(T,l)=\tau_l}
     {P;\Gamma\vdash e.l:S\tau_l}\;\text{(Project)}
$$

$$
\frac{P;\Gamma\vdash e:T\;S\overline{\alpha}
      \quad P;\Gamma\vdash v_i:S\tau_{l_i}}
     {P;\Gamma\vdash e\;\{\overline{l_i=v_i}\}:
        T\;S\overline{\alpha}}\;\text{(Update)}
$$

Update labels are distinct and belong to $T$. The same substitution appears in
the input, field types, and result, so an update cannot change type parameters.

Inference generates $\mathsf{Has}(\tau,l,\tau_r)$ before $T$ is known. If only
one record declares $l$, it chooses that record with fresh parameters. Otherwise
it waits for $\tau$'s head constructor. The solver repeats this process before
generalization. Any unresolved field is ambiguous (`E0245`), even if some
whole-program interpretation could choose a record. Field constraints never
enter a scheme. `(.l)` is checked as `\r -> r.l`, so the same restriction applies.

## Class Entailment and Declarations

$$
\frac{\pi\in P}{P\vdash\pi}\;\text{(Given)}
\quad
\frac{P\vdash C\;\tau\quad C\text{ has superclass }D}
     {P\vdash D\;\tau}\;\text{(Super)}
$$

$$
\frac{\mathsf{instance}\;Q\Rightarrow C\;\tau
      \quad P\vdash SQ}
     {P\vdash C\;S\tau}\;\text{(Instance)}
$$

The last rule uses a declared instance and a kind-correct substitution that
matches its head to the wanted predicate. Entailment of a set requires entailment
of every member. Superclass projection and instance application also construct
the dictionary evidence used during elaboration.

A class has one parameter. Its method signatures may mention that parameter but
may not add constraints of their own. Other method type variables are implicitly
quantified. A default method is checked at its declared type with the class
predicate given. Superclass relationships must be acyclic.

An instance head is a constructor applied to distinct variables. Its context
constrains only those variables. No two instance heads may overlap. Check each
method against the class method signature specialized to that head, using the
instance context as givens. Required superclass dictionaries must also be
obtainable. A missing method requires a default. Deriving generates instances
that undergo these same checks.

Data declarations introduce constructor schemes, with their parameters universally
quantified and their fields as successive arrow arguments. Constructor result
types name the declared datatype. The language has no existential variables,
GADT equations, or constructor-specific class contexts.

## Sugar, IO, and Whole Programs

These forms have no separate inference mechanism. Their expansions determine
their types:

| Surface form             | Typing account                                        |
| ------------------------ | ----------------------------------------------------- |
| `if b then x else y`     | `b : Bool`, and both branches have one type           |
| `a && b`, `a \|\| b`     | Both operands are `Bool`, with conditional evaluation |
| `a + b`, other operators | Application of the operator's scheme                  |
| `-e`                     | Application of `negate`                               |
| `(e op)`, `(op e)`       | A lambda applying the operator to a fresh parameter   |
| `[a..b]`                 | `enumFromTo a b`, with both bounds `Int`              |
| Lists, tuples, unit      | Applications of their data constructors               |
| `where`                  | A local binding group                                 |

For a monad constructor $m:*\to *$, a bind step requires an action of type
$m\;\alpha$ and a continuation whose result has type $m\;\beta$:

$$
\frac{P\vdash\mathsf{Monad}\;m
      \quad P;\Gamma\vdash e:m\;\alpha
      \quad\vdash p:\alpha\Rightarrow\Gamma_p
      \quad P;\Gamma,\Gamma_p\vdash s:m\;\beta}
     {P;\Gamma\vdash\mathsf{do}\;\{p\leftarrow e;\;s\}:m\;\beta}
     \;\text{(Do-Bind)}
$$

An expression statement uses `>>`, a `let` statement uses the binding rules,
and a final expression supplies the block's result. Refutable bind patterns
expand to a case with an `error` fallback. There is no `MonadFail` constraint.

`IO` is abstract to the source checker and has kind $*\to *$. It is an instance
of `Monad`, but a source program cannot equate `IO a` with `() -> a`. That
representation is exposed only during elaboration. Effectful primitives are
visible only while compiling the Prelude.

A complete program kind-checks its declarations, validates its class environment,
checks binding components in dependency order, and solves all remaining
constraints. Its entry point must have type `IO ()`. No unresolved class or field
constraint may survive. Top-level initializers run before that action, in
dependency order. These evaluation requirements complement the typing rules:
well typed does not mean terminating or free of explicit runtime errors.
