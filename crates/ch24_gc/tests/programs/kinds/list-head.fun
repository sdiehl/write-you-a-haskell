data Wrap f a = Wrap (f a)
f :: Wrap [] Int -> Int
f (Wrap (x : xs)) = x
_old = f (Wrap [42])

main = print _old
