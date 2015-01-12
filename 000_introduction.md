![](img/titles/introduction.png)

******

> When the limestone of imperative programming is worn away, the granite of
> functional programming will be observed.
> 
> <cite>-Simon Peyton Jones</cite>

<p class="halfbreak">
</p>

Introduction
============

Goals
-----

Off we go on our Adventure in Haskell Compilers! It will be intense, long,
informative, and hopefully fun.

It's important to stress several points about the goals before we start our
discussion:

a) This is not a rigorous introduction to type systems, it is a series of
informal discussions of topics structured around a reference implementation with
links provided to more complete and rigorous resources on the topic at hand. The
goal is to give you an overview of the concepts and terminology as well as a
simple reference implementation to play around with.

b) None of the reference implementations are industrial strength, many of them
gloss over fundamental issues that are left out for simplicity reasons. Writing
an industrial strength programming language involves work on the order of
hundreds of person-years and is an enormous engineering effort.

c) You should not use the reference compiler for anything serious. It is
intended for study and reference only.

Throughout our discussion we will stress the importance of semantics and the
construction of core calculi.  The frontend language syntax will be in the
ML-family syntax out of convenience rather than principle. Choice of lexical
syntax is arbitrary, uninteresting, and quite often distracts from actual
substance in comparative language discussion. If there is one central theme is
that the *design of the core calculus should drive development*, not the
frontend language.

Prerequisites
-------------

An intermediate understanding at the level of the *Real World Haskell* book is
recommended.  We will shy away from advanced type-level programming that is
often present in modern Haskell and will make heavy use of more value-level
constructs.  Although a strong familiarity with monads, monad transformers,
applicatives, and the standard Haskell data structures is strongly recommended.

Some familiarity with the standard 3rd party libraries will be useful. Many of
these are briefly overviewed in [What I Wish I Knew When Learning
Haskell](http://dev.stephendiehl.com/hask/).

In particular we will use:

* ``containers``
* ``unordered-containers``
* ``text``
* ``mtl``
* ``filepath``
* ``directory``
* ``process``
* ``parsec``
* ``pretty``
* ``wl-pprint``
* ``graphscc``
* ``haskeline``
* ``repline``
* ``cereal``
* ``deepseq``
* ``uniqueid``
* ``pretty-show``
* ``uniplate``
* ``optparse-applicative``
* ``unbound-generics``
* ``language-c-quote``
* ``bytestring``
* ``hoopl``
* ``fgl``
* ``llvm-general``
* ``smtLib``
* ``sbv``

In later chapters some experience with C, LLVM and x86 Assembly will be very
useful, although not strictly required.

Concepts
========

We are going to set out to build a *statically typed* *functional* programming
language with a *native code generation* *backend*. What does all this mean?

Functional Languages
--------------------

In mathematics a *function* is defined as a correspondence that assigns exactly
one element of a set to each element in another set.  If a function $f(x) = a$
then the function evaluated at $x$ will always have the value $a$. Central to
the notion of all mathematics is the notion is of *equational reasoning*, where
if $a= f(x)$ then for an expression $g(f(x), f(x))$, this is always equivalent
to $g(a, a)$. In other words the values computed by functions can always be
substituted freely at all occurrences.

The central idea of *functional programming* is to structure our programs in
such a way that we can reason about them as a system of equations just like one
we can in mathematics. The evaluation of a pure function is one which *side
effects* are prohibited, a function may only return a result without altering
the world in any *observable* way.

The implementation may perform effects, but central to this definition is the
unobservability of such effects.  A function is said to be *referentially
transparent* if replacing a function with its computed value output yields the
same observable behavior.

By contrast impure functions are ones which allow unrestricted and observable
side effects. The invocation of an impure function always allows for the
possibility of performing any functionality before yielding a value.

```javascript
// impure: mutation side effects
function f() {
  x += 3;
  return 42;
}

// impure: international side effects
function f() {
  launchMissiles();
  return 42;
}
```

The behavior of a pure function is independent of where and when it is
evaluated, whereas the sequence a impure function is intrinsically tied to its
behavior.

Functional programming is defined simply as programming strictly with pure
referentially transparent functions.

Static Typing
-------------

*Types* are a formal language integrated with a programming language that
refines the space of allowable behavior and degree of expressible programs for
the language.  Types are the world's most popular formal method for analyzing
programs.

$$
\begin{aligned}
1 &: \t{Nat} \\
(\lambda x . x) &: \forall a. a \to a \\
(\lambda x y . x) &: \forall a b. a \to b \to a \\
\end{aligned}
$$

In more sophisticated languages types and terms will commingle either with
explicit annotations on binders, or even as first class values themselves.

$$
\t{Pair} \ u \ v = \Lambda X . \lambda x^{U \rightarrow V \rightarrow X} . x u v
$$

In all the languages which we will implement the types present during compilation are
*erased*. Although they are present in the evaluation semantics, the runtime
cannot dispatch on types of values at runtime. Types by definition only exist at
compile-time in the static semantics of the language.

Functional Compilers
--------------------

A compiler is typically divided into parts, a *frontend* and a *backend*. These
are loose terms but the frontend typically deals with converting the human
representation of the code into some canonicalized form while the backend
converts the canonicalized form into another form that is suitable for
evaluation.

The high level structure of our functional compiler is described by the
following *block diagram*. Each describes a *phase* which is a sequence of
transformations composed to transform the input program.

<p class="center">
![](img/pipeline1.png)
</p>

* **Source**            - The frontend textual source language.
* **Parsing**           - Source is parsed into an abstract syntax tree.
* **Desugar**           - Redundant structure from the frontend language is removed and canonicalized.
* **Type Checking**     - The program is type-checked and/or type-inferred yielding an explicitly typed form.
* **Transformation**    - The core language is transformed to prepare for compilation.
* **Compilation**       - The core language is lowered into a form to be compiled or interpreted.
* **(Code Generation)** - Platform specific code is generated, linked into a binary.

A *pass* may transform the input program from one form into another or alter the
internal state of the compiler context. The high level description of the forms
our final compiler will go through is the following sequence:

<p class="center">
![](img/pipeline2.png)
</p>

Internal forms used during compilation are *intermediate representations* and
typically any non-trivial language will involve several.

Lexing
------

The source code is simply the raw sequence of text that specifies the program.
Lexing splits the text stream into a sequence of *tokens*. Only the presence of
invalid symbols is enforced, otherwise meaningless programs are accepted.
Whitespace is either ignored or represented as a unique token in the stream.

```haskell
let f x = x + 1
```

For instance the previous program might generate a token stream like the
following:

Token      Value
-----      -----
reserved   let
var        f
var        x
reservedOp =
var        x
reservedOp +
integer    1

Parsing
-------

A datatype for the *abstract syntax tree* (AST) is constructed by traversal of
the input stream and generation of the appropriate syntactic construct using a
parser.

```haskell
type Name = String

data Expr
  = Var Name
  | Lit Lit
  | Op PrimOp [Expr]
  | Let Name [Name] Expr

data Lit
  = LitInt Int

data PrimOp
  = Add
```

So for example the following string is parsed into the resulting ``Expr`` value.

```haskell
let f x = x + 1
```

```haskell
Let "f" ["x"] (Op Add [Var "x", Lit (LitInt 1)])
```

Desugaring
----------

Desugaring is the process by which the frontend AST is transformed into a
simpler form of itself by reducing the number of complex structures by
expressing them in terms of a fixed set of simpler constructs.

Haskell's frontend is very large and many constructs are simplified down. For
example ``where`` clauses and operation sections are the most common examples.
Where clauses are effectively syntactic sugar for let bindings and operator
sections are desugared into lambdas with the left or right
hand side argument assigned to a fresh variable.

Type Inference
--------------

Type inference is the process by which the untyped syntax is endowed with type
information by a process known as *type reconstruction* or *type inference*. The
inference process may take into account explicit user annotated types.

```haskell
let f x = x + 1
```

```haskell
Let "f" [] (Lam "x" (Op Add [Var "x", Lit (LitInt 1)]))
```

Inference will generate a system of constraints which are solved via a process
known as *unification* to yield the type of the expression.

```haskell
Int -> Int -> Int  ~  a -> b
b  ~  Int -> c
```

```haskell
f :: Int -> Int
```

In some cases this type will be incorporated directly into the AST and the
inference will transform the frontend language into an explicitly typed *core
language*.

```haskell
Let "f" []
  (Lam "x"
    (TArr TInt TInt)
    (App
      (App
        (Prim "primAdd") (Var "x"))
      (Lit (LitInt 1))))
```

Code Generation
---------------

From the core language we will either evaluate it on top of a high-level
interpreter written in Haskell itself, or into another intermediate language
like C or LLVM which can itself be compiled into native code.

```haskell
let f x = x + 1
```

Quite often this process will involve another intermediate representation which
abstracts over the process of assigning and moving values between CPU registers
and main memory. LLVM and GHC's Cmm are two target languages serving this
purpose.

```haskell
f:
  mov res, arg
  add res, 1
  ret
```

```haskell
define i32 @f(i32 %x) {
entry:
  %add = add nsw i32 %x, 1
  ret i32 %add
}
```

From here the target language can be compiled into the system's assembly
language. All code that is required for evaluation is *linked* into the
resulting module.

```perl
f:
	movl	%edi, -4(%rsp)
	movl	-4(%rsp), %edi
	addl	$1, %edi
	movl	%edi, %eax
	ret

```

And ultimately this code will be assembled into platform specific instructions by
the *native code generator*, encoded as a predefined sequence of CPU
instructions defined by the processor specification.

```perl
0000000000000000 <f>:
   0:	89 7c 24 fc          	mov    %edi,-0x4(%rsp)
   4:	8b 7c 24 fc          	mov    -0x4(%rsp),%edi
   8:	81 c7 01 00 00 00    	add    $0x1,%edi
   e:	89 f8                	mov    %edi,%eax
  10:	c3                   	retq
```

\pagebreak
