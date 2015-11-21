{-# LANGUAGE TypeOperators #-}

data Unit = Unit                 -- 1
data Empty                       -- 0
data (a * b) = Product a b       -- a * b
data (a + b) = Inl a | Inr b     -- a + b
data Exp a b = Exp (a -> b)      -- a^b
data Rec f   = Rec (f (Rec f))   --
