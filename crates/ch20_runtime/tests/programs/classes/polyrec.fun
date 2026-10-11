data Nested a = Done | More a (Nested [a])
size :: Nested a -> Int
size Done = 0
size (More x xs) = 1 + size xs
_old = size (More 1 (More [2] Done))

main = print _old
