f n x = if n == 0 then x else g (n-1) x
g n x = f n x
main = f 2 True
