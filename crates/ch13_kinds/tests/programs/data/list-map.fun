map f [] = []
map f (x : xs) = f x : map f xs
main = map (\x -> x + 1) [1,2,3]
