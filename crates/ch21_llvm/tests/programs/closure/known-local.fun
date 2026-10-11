f x = let { g n = if n==0 then x else g (n-1); } in g 10
main = print (f 42)
