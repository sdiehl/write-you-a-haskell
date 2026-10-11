equal :: Ord a => a -> a -> Bool
equal x y = x == y
_old = equal "a" "a"

main = print _old
