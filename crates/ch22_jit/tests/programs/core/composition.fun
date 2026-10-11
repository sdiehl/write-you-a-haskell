infixr 9 .
(.) f g x = f (g x)
_old = ((+ 1) . (+ 1)) 40

main = print _old
