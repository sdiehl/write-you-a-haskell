data Wrap f a = Wrap (f a)
un (Wrap x) = x
main = un (Wrap (Just 42))
