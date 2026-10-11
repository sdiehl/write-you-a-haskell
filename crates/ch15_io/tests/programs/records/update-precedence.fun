data P = P { x :: Int, y :: Int }
get p = p.x
_old = let p = P 3 4 in get p {x = 10}

main = print _old
