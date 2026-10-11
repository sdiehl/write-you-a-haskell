data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get :: P -> Int
get p = let result = p.x in result
main = get (P 3 4)
