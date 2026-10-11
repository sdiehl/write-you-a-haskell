same x y = x == y
main = (same :: Eq a => a -> a -> Bool) 1 1
