data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get :: P -> Int
get p = let result = p.x in result
_old = get (P 3 4)

main = print _old
