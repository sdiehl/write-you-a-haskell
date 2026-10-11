data T = A Int | B Int deriving (Eq, Ord, Show)
main = print (compare (A 9) (B 0), compare (A 1) (A 2), min (B 3) (B 2))
