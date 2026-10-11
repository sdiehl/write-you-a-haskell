data Pair a b = Pair a b
first (Pair x _) = x
_old = first ((Pair 41) True) + 1

main = print _old
