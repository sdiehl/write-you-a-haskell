data Wrap f a = Wrap (f a)
f :: Wrap ((->) Int) Int -> Int
f (Wrap g) = g 41
_old = f (Wrap (\x -> x + 1))

main = print _old
