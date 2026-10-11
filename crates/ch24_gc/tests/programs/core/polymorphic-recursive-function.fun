repeat n x = if n == 0 then x else repeat (n-1) x
_old = repeat 3 "ok"

main = print _old
