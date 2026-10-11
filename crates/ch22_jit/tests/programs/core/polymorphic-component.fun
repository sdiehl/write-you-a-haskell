f n x = if n == 0 then x else g (n-1) x
g n x = f n x
_old = f 2 True

main = print _old
