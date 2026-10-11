map f [] = []
map f (x : xs) = f x : map f xs
_old = map (\x -> x + 1) [1,2,3]

main = print _old
