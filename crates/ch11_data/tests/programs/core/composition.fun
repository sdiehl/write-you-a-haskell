infixr 9 .
(.) f g x = f (g x)
main = ((+ 1) . (+ 1)) 40
