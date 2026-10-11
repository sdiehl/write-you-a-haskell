data P = P { x :: Int, y :: Int }
get (P { y = n }) = n
main = get (P { x = 3, y = 4 })
