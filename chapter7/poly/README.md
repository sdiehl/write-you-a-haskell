Poly
====

A simple ML dialect with definitions, let polymorphism and a fixpoint operator.
Uses syntax directed HM type inference.

To compile and run:

```shell
$ cabal run
```

Usage:

```ocaml
Poly> let i x = x;
i : forall a. a -> a

Poly> i 3
3

Poly> :type i
i : forall a. a -> a

Poly> :type let k x y = x;
k : forall a b. a -> b -> a

Poly> :type let s f g x = f x (g x)
s : forall a b c. ((a -> b) -> c -> a) -> (a -> b) -> c -> b

Poly> :type let on g f = \x y -> g (f x) (f y)
on : forall a b c. (a -> a -> b) -> (c -> a) -> c -> c -> b

Poly> :type let let_bound = i (i i) (i 3)
let_bound : Int

Poly> :type let compose f g = \x -> f (g x)
compose : forall a b c. (a -> b) -> (c -> a) -> c -> b

Poly> let rec factorial n = 
  if (n == 0) 
  then 1
  else (n * (factorial (n-1)));
```

Notes
=====

Top level let declarations are syntactic sugar for nested lambda. For example: 

```ocaml
let add x y = x + y;
```

Is semantically equivalent to:

```ocaml
let add = \x -> \y -> x + y;
```

Top level Let-rec declarations are syntactic sugar for use of the ``fix``
operator. For example:

```ocaml
let rec factorial n = if (n == 0) then 1 else (n * (factorial (n-1)));
```
Is semantically equivalent to:

```ocaml
let factorial = fix (\factorial n -> if (n == 0) then 1 else (n * (factorial (n-1))));
```

License
=======

Released under MIT license.
