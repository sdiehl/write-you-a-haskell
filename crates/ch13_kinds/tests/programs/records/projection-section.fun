data P = P { x :: Int, y :: Int }
map f [] = []
map f (x:xs) = f x : map f xs
main = map (.x) [P 3 4,P 5 6]
