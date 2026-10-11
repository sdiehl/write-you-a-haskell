data Wrap f a = Wrap (f a)
f :: Wrap (Either String) Int -> Int
f (Wrap (Right x)) = x
_old = f (Wrap (Right 42))

main = print _old
