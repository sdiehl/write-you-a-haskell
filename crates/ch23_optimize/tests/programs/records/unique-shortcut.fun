data P = P { x :: Int, y :: Int }
get p = p.x
_old = get (P 3 4)

main = print _old
