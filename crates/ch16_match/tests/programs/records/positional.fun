data P = P { x :: Int, y :: Int }
get (P a b) = a + b
_old = get (P 3 4)

main = print _old
