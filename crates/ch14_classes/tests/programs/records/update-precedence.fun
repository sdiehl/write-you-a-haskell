data P = P { x :: Int, y :: Int }
get p = p.x
main = let p = P 3 4 in get p {x = 10}
