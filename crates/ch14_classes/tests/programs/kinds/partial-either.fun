data Wrap f a = Wrap (f a)
f :: Wrap (Either String) Int -> Int
f (Wrap (Right x)) = x
main = f (Wrap (Right 42))
