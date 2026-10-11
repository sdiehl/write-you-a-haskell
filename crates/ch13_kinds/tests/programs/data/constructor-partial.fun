data Pair a b = Pair a b
first (Pair x _) = x
main = first ((Pair 41) True) + 1
