data P = P { x :: Int, y :: Int }
twice n = n * 2
_old = let p = P 3 4 in twice p.x

main = print _old
