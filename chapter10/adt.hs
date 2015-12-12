{-# LANGUAGE TypeOperators #-}

data Unit = Unit                 -- 1
data Empty                       -- 0
data (a * b) = Product a b       -- a * b
data (a + b) = Inl a | Inr b     -- a + b
data Exp a b = Exp (a -> b)      -- a^b
data Rec f   = Mu (f (Rec f))    -- \mu

-- Products

type Prod3 a b c = a*(b*c)

data Prod3' a b c
  = Prod3 a b c

prod3 :: Prod3 Int Int Int
prod3 = Product 1 (Product 2 3)

-- Sums

type Sum3 a b c = (a+b)+c

data Sum3' a b c
  = Opt1 a
  | Opt2 b
  | Opt3 c

sum3 :: Sum3 Int Int Int
sum3 = Inl (Inl 2)

data Option a = None | Some a

type Option' a = Unit + a

some :: Unit + a
some = Inl Unit

none :: a -> Unit + a
none a = Inr a

-- Recursion

type Nat = Rec NatF
data NatF s = Zero | Succ s

zero :: Nat
zero = Mu Zero

succ :: Nat -> Nat
succ x = Mu (Succ x)

data ListF a b = Nil | Cons a b
type List a = Rec (ListF a)

-- syntactic sugar
nil :: List a
nil = Mu Nil

cons :: a -> List a -> List a
cons x y = Mu (Cons x y)

roll :: Rec f -> f (Rec f)
roll (Mu f) = f

unroll :: f (Rec f) -> Rec f
unroll f = Mu f

main = return ()
