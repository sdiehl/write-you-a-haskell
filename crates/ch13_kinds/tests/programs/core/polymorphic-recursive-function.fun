repeat n x = if n == 0 then x else repeat (n-1) x
main = repeat 3 "ok"
