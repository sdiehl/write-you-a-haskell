data Wrap f a = Wrap (f a)
f :: Wrap [] Int -> Int
f (Wrap (x : xs)) = x
main = f (Wrap [42])
