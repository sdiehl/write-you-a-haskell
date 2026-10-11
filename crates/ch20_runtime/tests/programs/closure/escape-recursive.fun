make bias = let { f n = if n==0 then bias else g (n-1); g n = f n; } in f
main = print (map (make 42) [0,10,20])
