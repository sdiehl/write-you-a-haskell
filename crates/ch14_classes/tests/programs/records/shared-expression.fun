data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get p = (p :: Q).x
main = get (Q "hi")
