even n = if n == 0 then True else odd (n-1)
odd n = if n == 0 then False else even (n-1)
_old = even 100

main = print _old
