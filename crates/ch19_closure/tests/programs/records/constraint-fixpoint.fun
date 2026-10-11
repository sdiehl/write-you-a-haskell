data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get p = (p.x,p.y)
_old = get (P 3 4)

main = print _old
