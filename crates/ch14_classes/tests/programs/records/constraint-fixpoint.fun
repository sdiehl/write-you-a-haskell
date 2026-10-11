data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get p = (p.x,p.y)
main = get (P 3 4)
