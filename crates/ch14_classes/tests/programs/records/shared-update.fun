data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
set :: P -> P
set p = p {x = 10}
main = set (P 3 4)
