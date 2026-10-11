f "hello" = 1
f _ = 2
g 'λ' = 3
g _ = 4
h (-2) = 5
h _ = 6
_old = (f "no",g 'λ',h (-2))

main = print _old
