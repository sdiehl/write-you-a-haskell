<p class="halfbreak">
</p>

Datatypes
=========

Algebraic data types
--------------------

**Algebraic datatypes** are a family of constructions arising out of two
operations, sums and products. A product encodes multiple arguments to
constructors and sums encode choice between constructors.

```haskell
data Unit = Unit                 -- 1
data Empty                       -- 0
data (a * b) = Product a b       -- a * b
data (a + b) = Inl a | Inr b     -- a + b
data Exp a b = Exp (a -> b)      -- a^b
data Rec f   = Rec (f (Rec f))   -- \mu
```

Syntax
------

GHC.Generics
------------

```haskell
class Generic a where
  type family Rep a :: * -> *
  to   :: a -> Rep a x
  from :: Rep a x -> a
```

Constructor  Models
-----------  -------
``V1``       Void: used for datatypes without constructors
``U1``       Unit: used for constructors without arguments
``K1``       Constants, additional parameters.
``:*:``      Products: encode multiple arguments to constructors
``:+:``      Sums: encode choice between constructors
``L1``       Left hand side of a sum.
``R1``       Right hand side of a sum.
``M1``       Meta-information (constructor names, etc.)

```haskell
newtype M1 i c f p = M1 (f p)
newtype K1 i c   p = K1 c
data U           p = U
```

```haskell
data (:*:) a b p = a p :*: b p
data (:+:) a b p = L1 (a p) | R1 (b p)
```

Full Source
-----------

\clearpage
