same x y = x == y
_old = (same :: Eq a => a -> a -> Bool) 1 1

main = print _old
