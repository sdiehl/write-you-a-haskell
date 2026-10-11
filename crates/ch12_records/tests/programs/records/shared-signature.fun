data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get :: P -> Int
get p = p.x
main = get (P 3 4)
