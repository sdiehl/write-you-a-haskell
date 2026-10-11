data P = P { x :: Int, y :: Int }
get p = p.x
main = get (P 3 4)
