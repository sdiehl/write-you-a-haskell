~~~~ {literal="latex_macros"}
~~~~

![](img/titles/evaluation.png)

******

<!--
<blockquote>
Well-typed programs cannot "go wrong".
<cite>Robin Milner</cite>
</blockquote>
-->

<p class="halfbreak">
</p>

Evaluation
==========

While the lambda calculus is exceedingly simple, there is a great deal of
variety in ways to evaluate and implement the reduction of lambda expressions.
The different models for evaluation are *evaluation stratgies*.

There is a bifurcation between two points in the design space: *strict* and
*non-strict* evaluation. An evaluation strategy is strict if the arguments to a
lambda expression are necessarily evaluated before a lambda is reduced.  A
language in which the arguments are not necessarily evaluated before a lambda is
reduced is non-strict.

Evaluation Models
-----------------

There many different models, and various hybrids thereof. We will consider three
dominant models:

* Call-by-value: arguments evaluated before function entered
* Call-by-name: arguments passed unevaluated
* Call-by-need: arguments passed unevaluated but an expression is only evaluated
  once and shared upon subsequent reference

Given an expression ``f x`` in the reduction in different evaluation models
proceeds differently:

*Call-by-value*:

1. Evaluate $x$ to $v$
2. Evaluate $f$ to $\lambda y. e$
3. Evaluate $[y/v]e$

*Call-by-name*:

1. Evaluate $f$ to $\lambda y. e$
2. Evaluate $[y/x]e$

*Call-by-need*:

1. Allocate a thunk $v$ for $x$
2. Evaluate $f$ to $\lambda y. e$
3. Evaluate $[y/v]e$

Terms that have a normal form in one model, may or may not have a normal form in
another. In call-by-need and call-by-name evaluation diverging terms are not
necessarily evaluated before entry, so some terms that have a normal form in
these models may diverge under call-by-value.

Call-by-value
-------------

Call by value is an extremely common evaluation model. Many programming
languages both imperative and functional use this evaluation strategy. The
essence of call-by-value is that there two categories of expressions: *terms*
and *values*. Values are lambda expressions and other terms which are in normal
form and cannot be reduced further. All arguments to lambda will be reduced to
normal form *before* they are bound inside the lambda and reduction only
proceeds once the argument is reduced.

For a simple arithmetic expression, the reduction proceeds as follows. Notice
how the subexpression ``(2 + 2)`` is evaluated to normal form before being
bound.

```haskell
(λx. λy. y x) (2 + 2) λx. x + 1 
=> (λy. y 4) λx. x + 1
=> (λy. x + 1) 4
=> 4 + 1
=> 5
```

Naturally there now are two evaluation rules for applications.

$$
\begin{array}{cl}
 \displaystyle \frac{e_1 \to e_1'}{e_1 e_2 \to e_1' e_2} & \trule{E-App1} \\ \\
 \displaystyle \frac{e_2 \to e_2'}{v_1 e_2 \to v_1 e_2'} & \trule{E-App2} \\ \\
 \displaystyle {(\lambda x . e) v \to [x / v] e } & \trule{E-AppLam} \\ \\
\end{array}
$$

Call-by-value interpreter
------------------------

For a simple little lambda calculus the call-by-value interpreter is quite
simple. Part of the runtime evaluation of lambda calculus involves the creation
of *closures*, environments which hold the locally variables in scope. In our
little language there are two possible values which reduction may converge on,
the **VInt** and **VClosure**.

```haskell
data Expr
  = Var Int
  | Lam Expr
  | App Expr Expr
  | Lit Int
  | Prim PrimOp Expr Expr
  deriving Show

data PrimOp = Add | Mul
  deriving Show
```

```haskell
data Value
  = VInt Int
  | VClosure Expr Env
  deriving Show

type Env = [Value]

emptyEnv :: Env
emptyEnv = []
```

The evaluator function simply maps the local scope and the term to the final
value. Whenever a variable is referred to it is lookup in the environment,
whenever a lambda is entered it extends the environment with the local scope of
the closure.

```haskell
eval :: Env -> Expr -> Value
eval env term = case term of
  Var n -> env !! n
  Lam a -> VClosure a env
  App a b ->
    let VClosure c env' = eval env a in
    let v = eval env b in
    eval (v : env') c

  Lit n -> VInt n
  Prim p a b -> (evalPrim p) (eval env a) (eval env b)

evalPrim :: PrimOp -> Value -> Value -> Value
evalPrim Add (VInt a) (VInt b) = VInt (a + b)
evalPrim Mul (VInt a) (VInt b) = VInt (a + b)
```

Call-by-name
------------

In call-by-name evaluation, the arguments to lambda expression are substituted
as is, evaluation simply proceeds from left to right substituting the outermost
lambda or reducing a value. If a substituted expression is not used it is never
evaluated.

$$
\begin{array}{cl}
 \displaystyle \frac{e_1 \to e_1'}{e_1 e_2 \to e_1' e_2} & \trule{E-App} \\ \\
 \displaystyle {(\lambda x . e) v \to [x / v] e } & \trule{E-AppLam} \\ \\
\end{array}
$$

For example, the same expression we looked at for call-by-value has the same
normal form but arrives at it by a different sequence of reductions:

```haskell
(λx. λy. y x) (2 + 2) λx. x + 1 
=> (λy.y (2 + 2)) λx. x + 1
=> (λx.x + 1) (2 + 2)
=> (2 + 2) + 1
=> 4 + 1
=> 5
```

Call-by-name is non-strict, although very few languages use this model.
[Frege](https://github.com/Frege/frege) being the most notable example.

Call-by-need
------------

*Call-by-need* is a special type of non-strict evaluation in which unevaluated
expressions are represented by suspensions or *thunks* which are passed into a
function unevaluated and only evaluated when needed or *forced*. When the thunk
is forced the representation of the thunk is *updated* with the computed value
and is not recomputed upon further reference.

The thunks for the unevaluated lambda expressions are allocated when evaluated
the resulting computed value is also placed in the same reference so that
subsequent computations share the result. If the argument is never needed it is
never computed, this results in a trade-off between space and time. 

<!--
Evaluation for call-by-need never has worse asymptotic time complexity than
call-by-value, but can result in worse space complexity.
-->

Since the evaluation of subexpression is not in any pre-ordained order, any
impure functions with side-effects will be evaluated in an unspecified order, as
a result call-by-need can only effectively be implemented in a purely functional
setting.

```haskell
type Thunk = () -> IO Value

data Value
  = VBool Bool
  | VInt Integer
  | VClosure (Thunk -> IO Value)
```

```haskell
update :: IORef Thunk -> Value -> IO ()
update ref v = do
  writeIORef ref (\() -> return v)
  return ()
```

```haskell
force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v <- th ()
  update ref v
  return v
```

```haskell
mkThunk :: Env -> String -> Expr -> (Thunk -> IO Value)
mkThunk env x body = \a -> do
  a' <- newIORef a
  eval ((x, a') : env) body
```

```haskell
eval :: Env -> Expr -> IO Value
eval env ex = case ex of
  EVar n -> do
    th <- lookupEnv env n
    v <- force th
    return v

  ELam x e -> return $ VClosure (mkThunk env x e)

  EApp a b -> do
    VClosure c <- eval env a
    c (\() -> eval env b)

  EBool b -> return $ VBool b
  EInt n  -> return $ VInt n
  EFix e  -> eval env (EApp e (EFix e))
```

For example, in this model the following program will not diverge since the
omega combinator passed into the constant function is not used and therefore the
argument is not evaluated.

```haskell
omega = (\x -> x x) (\x -> x x)
test1 = (\y -> 42) omega
```

```haskell
omega :: Expr
omega = EApp (ELam "x" (EApp (EVar "x") (EVar "x")))
             (ELam "x" (EApp (EVar "x") (EVar "x")))

test1 :: IO Value
test1 = eval [] $ EApp (ELam "y" (EInt 42)) omega
```

Higher Order Interpreters
=========================

HOAS
----

Haskell being a rich language has a variety of extensions that, among other
things, allow us to map lambda expressions in our defined language directly onto
lambda expressions in Haskell. In this case we will use a GADT to embed a
Haskell expression inside of our expression type.

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
  Lift :: a                       -> Expr a
  Tup  :: Expr a -> Expr b        -> Expr (a, b)
  Lam  :: (Expr a -> Expr b)      -> Expr (a -> b)
  App  :: Expr (a -> b) -> Expr a -> Expr b
  Fix  :: Expr (a -> a)           -> Expr a
```

The most notable feature of this encoding is that there is no distinct
constructor for variables. Instead they are simply as values in the host
language. Some example expressions:

```haskell
id :: Expr (a -> a)
id = Lam (\x -> x)

tr :: Expr (a -> b -> a)
tr = Lam (\x -> (Lam (\y -> x))) 

fl :: Expr (a -> b -> b)
fl = Lam (\x -> (Lam (\y -> y))) 
```

Our evaluator then simply uses Haskell for evaluation.

```haskell
eval :: Expr a -> a
eval (Lift v)    = v
eval (Tup e1 e2) = (eval e1, eval e2)
eval (Lam f)     = \x -> eval (f (Lift x))
eval (App e1 e2) = (eval e1) (eval e2)
eval (Fix f)     = (eval f) (eval (Fix f))
```

Some examples of use:

```haskell
fact :: Expr (Integer -> Integer)
fact =
  Fix (
    Lam (\f ->
      Lam (\y ->
        Lift (
          if eval y == 0
          then 1
          else eval y * (eval f) (eval y - 1)))))

test :: Integer
test = eval fact 10

main :: IO ()
main = print test
```

Several caveats must be taken when working with HOAS, first that is a bit more
work to transform expressions in this form since in order to work with the
expression we would need to reach under the lambda binder of a Haskell function
itself. Since all the machinery is wrapped up inside of Haskell's implementation
even simple operations like pretty printing and writing transformation passes
can be more difficult. This form is a good form for evaluation, but not for
transformation.

PHOAS
-----

A slightly different form of HOAS called PHOAS ( Parametric Higher Order
Abstract Syntax ) uses lambda representation parameterized over the binder type
under an existential type. 

```haskell
{-# LANGUAGE RankNTypes #-}

data ExprP a
  = VarP a
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Integer

newtype Expr = Expr { unExpr :: forall a . ExprP a }
```

The lambda in our language is simply a lambda within Haskell. So for example,
the usual SK combinators would be written as follows:

```haskell
i :: ExprP a
i = LamP (\a -> VarP a)

k :: ExprP a
k = LamP (\x -> LamP (\y -> VarP x))

s :: ExprP a
s = LamP (\x -> LamP (\y -> LamP (\z -> AppP (AppP (VarP x) (VarP z)) (AppP (VarP y) (VarP z)))))
```

Evaluation will result in a runtime ``Value`` type, just as before with our
outer interpreters. We will use several "extractor" function which use
incomplete patterns under the hood. The model itself does not prevent malformed
programs from blowing up here, and so it is necessary to guarantee that the
program is sound before evaluation. Normally this would be guaranteed at a
higher level by a typechecker before even reaching this point.

```haskell
data Value
  = VLit Integer
  | VFun (Value -> Value)

fromVFun :: Value -> (Value -> Value)
fromVFun val = case val of
  VFun f -> f
  _      -> error "not a function"

fromVLit :: Value -> Integer
fromVLit val = case val of
  VLit n -> n
  _      -> error "not a integer"
```

Evaluation simply exploits the fact that nestled up under our existential type
is just a Haskell function and so we get all the name capture, closures and
binding machinery for free. The evaluation logic for PHOAS model is extremely
short.

```haskell
eval :: Expr -> Value
eval e = ev (unExpr e) where
  ev (LamP f)      = VFun(ev . f)
  ev (VarP v)      = v
  ev (AppP e1 e2)  = fromVFun (ev e1) (ev e2)
  ev (LitP n)      = VLit n
```

So for a complete consider the ``S K K = I`` example again and check the result: 

```haskell
skk :: ExprP a
skk = AppP (AppP s k) k

example :: Integer
example = fromVLit $ eval $ Expr (AppP skk (LitP 3))
```

We will use the evaluation technique Ly extensively for writing interpreters for
our larger languages. It is an extremely convenient and useful method for
writing interpreters in Haskell.

Embedding IO
------------

As mentioned before, effects are first class values in Haskell.

In Haskell we don't read from a file directly, but create a value that stands in
for reading from a file. This allows us to very cleanly model an interpreter for
our language inside of Haskell by establishing a mapping between the base
operations of our language and existing function implementations of the
standard operations in Haskell, and using monadic operations to build up a
pure effectful computation as a result of interpretation. Then after
evaluation, we finally lift the resulting IO value up Haskell and execute the
results.  This fits in nicely with the PHOAS model and allows us to
efficiently implement fully-fledged interpreter for our language with
remarkably little code, simply by exploiting Haskell's implementation.

To embed IO actions inside of our interpreter we create a distinct ``VEffect``
value that will build up a sequenced IO computation during evaluation and then
this value will be passed off to Haskell and reified into real world effects.

```haskell
data ExprP a
  = VarP a
  | GlobalP Name
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Char
  | EffectP a

data Value
  = VChar Char
  | VFun (Value -> Value)
  | VEffect (IO Value)
  | VUnit

fromVEff :: Value -> (IO Value)
fromVEff val = case val of
  VEffect f -> f
  _         -> error "not a effect"
```

```haskell
eval :: Expr -> Value
eval e = ev (unExpr e) where
  ev (LamP f)      = VFun(ev . f)
  ev (AppP e1 e2)  = fromVFun (ev e1) (ev e2)
  ev (LitP n)      = VChar n
  ev (EffectP v)   = v
  ev (VarP v)      = v
  ev (GlobalP op)  = prim op

-- Lift an effect from our language into Haskell IO.
run :: Expr -> IO ()
run f = void (fromVEff (eval f))
```

The ``prim`` function will simply perform the lookup on the set of builtin
operations, which we'll define with a bit of syntactic sugar for wrapping up
Haskell functions.

```haskell
unary :: (Value -> Value) -> Value
unary f = lam $ \a -> f a

binary :: (Value -> Value -> Value) -> Value
binary f = lam $ \a ->
           lam $ \b -> f a b

prim :: Name -> Value
prim op = case op of
 "putChar#" -> unary $ \x ->
    VEffect $ do
      putChar (fromVChar x)
      return VUnit

 "getChar#" -> VEffect $ do
      val <- getChar
      return (VChar val)

 "bindIO#"   -> binary $ \x y -> bindIO x y
 "returnIO#" -> unary  $ \x   -> returnIO x
 "thenIO#"   -> binary $ \x y -> thenIO x y
```

For example ``thenIO#`` sequences effects in our language will simply squash two
``VEffect`` objects into one composite effect building up a new ``VEffect``
value that is using Haskell's monadic sequencing on the internal ``IO`` value.

```haskell
bindIO :: Value -> Value -> Value
bindIO (VEffect f) (VFun g) = VEffect (f >>= fromVEff . g)

thenIO :: Value -> Value -> Value
thenIO (VEffect f) (VEffect g) = VEffect (f >> g)

returnIO :: Value -> Value
returnIO a = VEffect $ return a
```

Effectively we're just recreating the same conceptual relationship that Haskell
IO has with it's runtime, but instead our host language uses Haskell as the
runtime!

Full Source
===========

**Evaluation**

* [Call-by-value](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter6/interp.hs)
* [Call-by-need](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter6/lazy.hs)

**Higher Order Interpreters**

* [HOAS](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter6/hoas.hs)
* [PHOAS](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter6/phoas.hs)
* [Embedding IO](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter6/io.hs)
