data P = P { x :: Int, y :: Int }
get (P a b) = a + b
main = get (P 3 4)
