data P = P { x :: Int, y :: Int }
get (P { y = n }) = n
_old = get (P { x = 3, y = 4 })

main = print _old
