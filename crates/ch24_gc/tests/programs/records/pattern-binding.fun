data P = P { x :: Int, y :: Int }
P { y = n } = P 3 4
_old = n

main = print _old
