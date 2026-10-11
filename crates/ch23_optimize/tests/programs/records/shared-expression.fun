data P = P { x :: Int, y :: Int }
data Q = Q { x :: String }
get p = (p :: Q).x
_old = get (Q "hi")

main = print _old
