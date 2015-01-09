![](img/titles/type_systems.png)

******

<blockquote>
[A type system is a] tractable syntactic method for proving the
absence of certain program behaviors by classifying phrases
according to the kinds of values they compute.
<cite>Benjamin Pierce</cite>
</blockquote>

<p class="halfbreak">
</p>

Type Systems
============

Type systems are a formal language in which we can describe and restrict the
semantics of a programming language. The study of the subject is a rich and open
area of research with many degrees of freedom in the design space.

*As stated in the introduction, this is a very large topic and we are only going
to cover enough of it to get through writing the type checker for our language,
not the subject in its full generality.* The classic text that everyone reads
is *Types and Programming Languages* or ( TAPL ) and discusses the topic more in
depth. In fact we will follow TAPL very closely with a bit of a Haskell flavor.

Rules
-----

In the study of programming language semantics, logical statements are written
in a specific logical notation. A property, for our purposes this will be a fact
about the type of a term, will be written as follows:

$$
1 : \t{Nat}
$$

These facts exist within a preset universe of discourse called a *type system*
with definitions, properties, conventions, and rules of logical deduction about
types and terms. Within a given system, we will have several properties about
these terms. For example:

* **(A1)** 0 is a natural number.
* **(A2)** For a natural number $n$, $\mathtt{succ}(n)$ is a natural number.

Given several properties about natural numbers, we'll use a notation that will
allow us to chain them together to form proofs about arbitrary terms in our
system.

$$
\begin{array}{cl}
\infrule{}{0 : \mathtt{Nat}} & \trule{A1} \\ \\
\infrule{n : \mathtt{Nat}}{\mathtt{succ}(n) : \mathtt{Nat}} & \trule{A2} \\ \\
\end{array}
$$

In this notation, the expression above the line is called the *antecedent*, and
expression below the line is called the *conclusion*. A rule with no antecedent
is an *axiom*.

The variable $n$ is *metavariable* standing for any natural number, an instances
of a rule is a substitution of values for these metavariables. A *derivation* is
a tree of rules of finite depth. We write $\vdash C$ to indicate that there
exists a derivation whose conclusion is $C$, that $C$ is provable.

For example with $\vdash 2 : \t{Nat}$ by the derivation.

$$
\dfrac
  {
    \quad
    \dfrac
    {
      \quad
      \dfrac{}
      {
        0 : \t{Nat}
      }
      \trule{A1}
    }
    {
      \t{succ}(0) : \t{Nat}
    }
    \trule{A2}
  }
  {
    \t{succ}(\t{succ}(0)) : \t{Nat}
  }
  \trule{A2}
$$

Also present in these derivations may be a *typing context* or *typing
environment* written as $\Gamma$. The context is a sequence of named variables
mapped to properties about the named variable. The comma operator for the
context extends $\Gamma$ by adding a new property on the right of the existing
set. The empty context is denoted $\varnothing$ and is the terminal element in
this chain of properties that carries no information. For example:

$$
\begin{aligned}
\Gamma ::=\ & \varnothing \\
            & \Gamma,\  x : \tau  \\
\end{aligned}
$$

$$
\frac{\Gamma \vdash e_1 : \t{Nat} \quad \Gamma \vdash e_2 :
  \t{Nat}}{\Gamma \vdash e_1 + e_2 : \t{Nat}}
$$

In the case where the property is always implied regardless of the context we
will shorten the expression. This is just a lexical convention.

$$
{\varnothing \vdash P} \quad \quad := \quad {\vdash P}
$$

Type Safety
-----------

In the context of modeling the semantics of programming languages using this
logical notation, we often refer to two fundamental categories of rules of the
semantics.

* **Statics** : Semantic descriptions which are derived from the syntax of the language.
* **Dynamics** : Semantics descriptions which describe the value evolution resulting from a program.

*Type safety* is defined to be the equivalence between the statics and the
dynamics of the language. This equivalence is modeled by two properties that
relate the types and evaluation semantics:

* **Progress** :  If an expression is well typed then either it is a value, or it can be further evaluated by an available evaluation rule.
* **Preservation** : If an expression $e$ has type $\tau$, and is evaluated to $e'$, then $e'$ has type $\tau$.

Types
-----

The word "type" is quite often overload in the common programming lexicon.
Other languages often refer to runtime tags present in the dynamics of the
languages as "types". Some examples:

```bash
# Python
>>> type(1)
<type 'int'>

# Javascript
> typeof(1)
'number'

# Ruby
irb(main):001:0> 1.class
=> Fixnum

# Julia
julia> typeof(1)
Int64

# Clojure
user=> (type 1)
java.lang.Long
```

While this is a perfectly acceptable alternative definition, we are not going to
go that route and restrict ourselves purely to the discussion of *static types*.
Under this set of definitions many of so-called dynamically typed language often
only have a single static type. For instance in Python all static types are
subsumed by the ``PyObject``, and it is only at runtime that the tag
``PyTypeObject *ob_type`` is discriminated on to give rise to Python notion of
"types". Again, this is not the kind of type we will discuss. The tradeoffs that
these languages make is that they often have trivial static semantics while the
dynamics for the language are often exceedingly complicated. Languages like
Haskell and OCaml are the opposite point in this design space.

Types will usually be written as $\tau$ and can consist of many different
constructions to the point where the type language may become as rich as the
value level language. For now let's only consider three simple types, two
*ground types* ($\t{Nat}$ and $\t{Bool}$) and an *arrow type*.

$$
\begin{aligned}
\tau ::=\ & \t{Bool} \\
          & \t{Nat} \\
          & \tau \rightarrow \tau \\
\end{aligned}
$$

The arrow type will be the type of function expressions, the left argument being
the input type and the output type on the right. The arrow type will by convention
associate to the right.

$$
\tau_1 \to \tau_2 \to \tau_3 \to \tau_4 \quad = \quad \tau_1 \to (\tau_2 \to (\tau_3 \to \tau_4))
$$

Small-Step Notation
-------------------

The real quantity we're interested in formally describing is expressions in
programming languages.

A programming language semantics are described by the *operational semantics* of
the language. The operational semantics can be thought of as a description of an
abstract machine which operates over the abstract terms of the programming
language in the same way that a virtual machine might operate over instructions.
It is a framework for modeling the aspects of the runtime behavior of the
program before running it by describing the transitions terms may take.

We use a framework called *small-step semantics* where a deviation shows how
individual rewrites compose to produce a term, which can evaluate to a value
through a sequence of state changes of an abstract machine. Ultimately we'd like
the term to transition and terminate to a *value* in our language instead of
becoming "stuck" as we encountered before.

Recall our little calculator language from before when we constructed our first
parser:

```haskell
data Expr
  = Tr
  | Fl
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  | Zero
```

The expression syntax is as follows:

$$
\begin{aligned}
e ::=\ & \t{True} \\
       & \t{False}  \\
       & \t{iszero}\ e \\
       & \t{succ}\ e \\
       & \t{pred}\ e \\
       & \ite{e}{e}{e} \\
       & 0  \\
\end{aligned}
$$

The small step evaluation semantics for this little language are uniquely
defined by the following 9 rules. They describe each step that an expression may
take during evaluation which may or may not terminate and converge on a value.

$$
\begin{array}{cl}
   \displaystyle \frac{e_1 \to e_2}{\t{succ}\ e_1 \to \t{succ}\ e_2} & \trule{E-Succ} \\ \\
   \displaystyle \frac{e_1 \to e_2}{\t{pred}\ e_1 \to \t{pred}\ e_2} & \trule{E-Pred} \\ \\
   \displaystyle \t{pred}\ 0 \to 0 & \trule{E-PredZero} \\ \\
   \displaystyle \t{pred}\ (\t{succ}\ n) \to n & \trule{E-PredSucc} \\ \\
   \displaystyle \frac{e_1 \to e_2}{\t{iszero}\ e_1 \to \t{iszero}\ e_2} & \trule{E-IsZero} \\ \\
   \displaystyle \t{iszero}\ 0 \to \t{true} & \trule{E-IsZeroZero} \\ \\
   \displaystyle \t{iszero}\ (\t{succ}\ n) \to \t{false} & \trule{E-IsZeroSucc} \\ \\
   \displaystyle \ite{\t{True}}{e_2}{e_3} \rightarrow e_2 & \trule{E-IfTrue} \\ \\
   \displaystyle \ite{\t{False}}{e_2}{e_3} \rightarrow e_3 & \trule{E-IfFalse} \\ \\
\end{array}
$$

The evaluation logic for our interpreter simply reduced an expression by the
predefined evaluation rules until either it reached a normal norm ( a value ) or
because it got stuck.

```haskell
nf :: Expr -> Expr
nf t = fromMaybe t (nf <$> eval1 t)

eval :: Expr -> Maybe Expr
eval t = case isVal (nf t) of
  True  -> Just (nf t)
  False -> Nothing -- term is "stuck"
```

Values in our language are defined to be literal numbers or booleans.

```haskell
isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False
```

Written in applicative form there is a noticeable correspondence between each of
the evaluation rules and our evaluation logic.

```haskell
-- Evaluate a single step.
eval1 :: Expr -> Maybe Expr
eval1 expr = case expr of
  Succ t                    -> Succ <$> (eval1 t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval1 t)
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval1 t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval1 t
  _                         -> Nothing
```

As we noticed before we could construct all sorts of pathological expressions
that would become stuck. Looking at the evaluation rules, each of the guarded
pattern matches gives us a hint of where things might "go wrong" whenever a
boolean is used in the place of a number and vice versa. We'd like to statically
enforce this invariant at compile-time instead, and so we'll introduce a small
type system to handle the two syntactic categories of terms that exist. The
abstract type of natural numbers and the type of booleans:

$$
\begin{aligned}
\tau ::=\ & \t{Bool} \\
          & \t{Nat} \\
\end{aligned}
$$

Which is implemented in Haskell as the following datatype:

```haskell
data Type
  = TBool
  | TNat
```

Now for the typing rules:

$$
\begin{array}{cl}
 \displaystyle \frac{e_1 : \t{Nat}}{\t{succ}\ e_1 : \t{Nat}} & \trule{T-Succ} \\ \\
 \displaystyle \frac{e_1 : \t{Nat}}{\t{pred}\ e_1 : \t{Nat}} & \trule{T-Pred} \\ \\
 \displaystyle \frac{e_1 : \t{Nat}}{\t{iszero}\ e_1 : \t{Bool}} & \trule{T-IsZero} \\ \\
 \displaystyle 0 : \t{Nat} & \trule{T-Zero} \\ \\
 \displaystyle \t{True} : \t{Bool} & \trule{T-True} \\ \\
 \displaystyle \t{False} : \t{Bool} & \trule{T-False} \\ \\
 \displaystyle
   \frac{\Gamma \vdash e_1 : \t{Bool} \quad \Gamma \vdash e_2 : \tau
     \quad \Gamma \vdash e_3 : \tau}{\Gamma \vdash \ite{e_1}{e_2}{e_3} :
     \tau} & \trule{T-If} \\ \\
\end{array}
$$

These rules restrict the space of all possible programs. It is more involved to
show, but this system has both progress and preservation as well. If a term is
now well-typed it will always evaluate to a value and cannot "go wrong" at
evaluation.

To check the well-formedness of an expression we implement a piece of logic
known as *type checker* which determines whether the term has a well-defined
type in terms of typing rules, and if so returns it or fails with an exception
in the case where it does not.

```haskell
type Check a = Except TypeError a

data TypeError
  = TypeMismatch Type Type

check :: Expr -> Either TypeError Type
check = runExcept . typeof
```

```haskell
typeof :: Expr -> Check Type
typeof expr = case expr of
  Succ a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  Pred a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  IsZero a -> do
    ta <- typeof a
    case ta of
      TNat -> return TBool
      _    -> throwError $ TypeMismatch ta TNat

  If a b c -> do
    ta <- typeof a
    tb <- typeof b
    tc <- typeof c
    if ta /= TBool
    then throwError $ TypeMismatch ta TBool
    else
      if tb /= tc
      then throwError $ TypeMismatch ta tb
      else return tc

  Tr   -> return TBool
  Fl   -> return TBool
  Zero -> return TNat

```

Observations
------------

The pathological stuck terms that we encountered previously in our untyped
language are now completely inexpressive and are rejected at compile-time.

```ocaml
Arith> succ 0
succ 0 : Nat

Arith> succ (succ 0)
succ (succ 0) : Nat

Arith> if false then true else false
false : Bool

Arith> iszero (pred (succ (succ 0)))
false : Bool

Arith> pred (succ 0)
0 : Nat

Arith> iszero false
Type Mismatch: Bool is not Nat

Arith> if 0 then true else false
Type Mismatch: Nat is not Bool
```

This is good, we've made a whole class of illegal programs unrepresentable. Lets
do more of this!

Simply Typed Lambda Calculus
----------------------------

The *simply typed lambda calculus* ( STLC ) of Church and Curry is an extension
of the lambda calculus that annotates each lambda binder with a type term. The
STLC is *explictly typed*, all types are present directly on the binders and to
determine the type of any variable in scope we only need to traverse to its
enclosing scope.

$$
\begin{aligned}
e :=\ & x \\
     & e_1\ e_2 \\
     & \lambda x : \tau . e \\
\end{aligned}
$$

The simplest STLC language is these three terms, however we will add
numeric and boolean literal terms so that we can write meaningful
examples.

$$
\begin{aligned}
e :=\ & x \\
     & e_1\ e_2 \\
     & \lambda x : \tau . e \\
     & n \\
     & \t{true} \\
     & \t{false} \\
     & \ite{e}{e}{e} \\
\end{aligned}
$$

We can consider a very simple type system for our language that will consist of ``int``
and ``Bool`` types and function types.

$$
\begin{aligned}
\tau :=\ & \t{int} \\
         & \t{Bool} \\ 
         & \tau \rightarrow \tau \\
\end{aligned}
$$

Type Checker
------------

The typing rules are quite simple, and again we get the nice property that there is
a one-to-one mapping between each syntax term and a typing rule.

* **T-Var** Variables are simply pulled from the context.
* **T-Lam** lambdas introduce a typed variable into the environment when
  inferring the body.
* **T-App** Applications of a lambda with type ``t1 -> t2`` to a value of type
  ``t1`` yields a value of type ``t2``.

$$
\begin{array}{cl}
 \displaystyle \frac{x:\sigma \in \Gamma}{\Gamma \vdash x:\sigma} & \trule{T-Var} \\ \\
 \displaystyle \infrule{\Gamma, x : \tau_1 \vdash e : \tau_2}{\Gamma \vdash \lambda x . \tau_2 : e_1 \rightarrow e_2 } & \trule{T-Lam} \\ \\
 \displaystyle \infrule{\Gamma \vdash e_1 : \tau_1 \rightarrow \tau_2 \andalso \Gamma \vdash e_2 : \tau_1}{\Gamma \vdash e_1 e_2 : \tau_2} & \trule{T-App} \\ \\
 \displaystyle
   \frac{\Gamma \vdash c : \t{Bool} \quad \Gamma \vdash e_1 : \tau
     \quad \Gamma \vdash e_2 : \tau}{\Gamma \vdash \ite{c}{e_1}{e_2} :
     \tau} & \trule{T-If} \\ \\
 \displaystyle \Gamma \vdash n : \t{int} & \trule{T-Int} \\ \\
 \displaystyle \Gamma \vdash \t{True} : \t{Bool} & \trule{T-True} \\ \\
 \displaystyle \Gamma \vdash \t{False} : \t{Bool} & \trule{T-False} \\ \\
\end{array}
$$

The evaluation rules describe the nature by which values transition between
other values and determine the runtime behavior of the program.

$$
\begin{array}{cl}
 \displaystyle \frac{e_1 \to e_1'}{e_1 e_2 \to e_1' e_2} & \trule{E-App1} \\ \\
 \displaystyle \frac{e_2 \to e_2'}{v_1 e_2 \to v_1 e_2'} & \trule{E-App2} \\ \\
 \displaystyle {(\lambda x: \tau . e_1) v_2 \to [x / v_2] e_1 } & \trule{E-AppLam} \\ \\
 \displaystyle \ite{\t{True}}{e_2}{e_3} \rightarrow e_2 & \trule{E-IfTrue} \\ \\
 \displaystyle \ite{\t{False}}{e_2}{e_3} \rightarrow e_3 & \trule{E-IfFalse} \\ \\
 \displaystyle \frac{e_1 \to e_1'}{\ite{e_1}{e_2}{e_3} \to \ite{e_1'}{e_2}{e_3}} & \trule{E-If} \\ \\
\end{array}
$$

Since we now have the notion of scoped variables for lambda, we will implement a
typing environment ``Env`` as manifest as $\Gamma$ in our typing rules.

```haskell
type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend xt env = xt : env

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x
```

The typechecker will be a ``ExceptT`` + ``Reader`` monad transformer stack, with
the reader holding the typing environment. There are three possible failure
modes for the our simply typed lambda calculus typechecker:

* The case when we try to unify two unlike types.
* The case when we try to apply a non-function to an argument.
* The case when a variable is referred to that is not in scope.

```haskell
data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

type Check = ExceptT TypeError (Reader Env)
```

There is a direct equivalence between syntax patterns here and the equivalent
typing judgement for it. This will not always be the case in general though. The
implementation of the type checker is as follows:

```haskell
check :: Expr -> Check Type
check expr = case expr of

  Lit (LInt{}) -> return TInt

  Lit (LBool{}) -> return TBool

  Lam x t e -> do
    rhs <- inEnv (x,t) (check e)
    return (TArr t rhs)

  App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
       (TArr a b) | a == t2 -> return b
       (TArr a _) -> throwError $ Mismatch t2 a
       ty -> throwError $ NotFunction ty

  Var x -> lookupVar x
```

Evaluation
----------

Fundamentally the evaluation of the typed lambda calculus is no different than
the untyped lambda calculus, nor could it be since the syntactic addition of
types is purely a static construction and cannot have any manifestation at
runtime by definition. The only difference is that the simply typed lambda
calculus admits strictly less programs than the untyped lambda calculus.

The foundational idea in compilation of static typed languages is that a typed
program can be transformed into an untyped program that *erases* type
information but preserves the evaluation semantics of the typed program. If our
program has *type safety* then it can never "go wrong" at runtime.

Of course the converse is not true, programs that do not "go wrong" are not
necessarily well-typed, although whether we can prove whether a non well-typed
program cannot go wrong is an orthogonal issue. The game that we as statically
typed language implementors play is fundamentally one of restriction: we take
the space of all programs and draw a large line around the universe of discourse of
programs that we are willing to consider, since these are the only programs that
we can prove properties for.

<blockquote>
Well-typed programs don't go wrong, but not every program that never goes wrong
is well-typed. It's easy to exhibit programs that don't go wrong but are
ill-typed in ... any ... decidable type system. Many such programs are useful,
which is why dynamically-typed languages like Erlang and Lisp are justly
popular.
<cite>Simon Peyton Jones</cite>
</blockquote>

<!--
<p class="center">
![](img/abysmal_pain.png)
</p>
-->

Power always comes at a price. Using one system you can do more things. In
another you can say more about the things a program can do. The fundamental art
in the discipline of language design is balancing the two to find the right
power-to-weight ratio.

Observations
------------

Some examples to try:

```bash
Stlc> (\x : Int . \y : Int . y) 1 2
2

Stlc> (\x : (Int -> Int). x) (\x : Int . 1) 2
1

Stlc> (\x : Int . x) False
Couldn't match expected type 'Int' with actual type: 'Bool'
```

```bash
Stlc> 1 2
Tried to apply to non-function type: Int

Stlc> (\x : Int . (\y : Int . x))
<<closure>>
```

Notation Reference
------------------

The notation introduced here will be used throughout the construction of the
Haskell compiler. For reference here is a list of each of the notational
conventions we will use. Some of these terms are not yet introduced.

<p class="halfbreak">
</p>

Notation                        Convention
-----------                     ------------
$\{ a, b, c \}$                 Set
$\overline{\alpha}$             Vector
$x : \tau$                      Type judgement
$P(x)$                          Predicate
$P(x) : Q(x)$                   Conditional
$P \vdash Q$                    Implication
$\alpha,\beta$                  Type variables
$\Gamma$                        Type context
$x, y, z$                       Expression variables
$e$                             Expression metavariable
$\tau$                          Type metavariable
$\kappa$                        Kind metavariable
$\sigma$                        Type scheme metavariable
$C$                             Type constraint
$\alpha \sim \beta$             Unification constraint
$[\alpha / \beta]$              Substitution
$[s] x$                         Substitution application
$s$                             Substitution metavariable
$a \rightarrow b$               Function type
$C \Rightarrow b$               Qualified type
$a \times b$                    Product type
$a + b$                         Sum type
$\bot$                          Bottom type
$\forall \alpha. \tau$          Universal quantifier
$\exists \tau$                  Existential quantifier
$\mathtt{Nat}, \mathtt{Bool}$   Ground type

<p class="halfbreak">
</p>

Full Source
-----------

* [Typed Arithmetic](https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter5/calc_typed)
* [Simply Typed Lambda Calculus](https://github.com/sdiehl/write-you-a-haskell/tree/master/chapter5/stlc)

\pagebreak
