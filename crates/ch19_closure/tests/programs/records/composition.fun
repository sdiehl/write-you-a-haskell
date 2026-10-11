data P = P { x :: Int, y :: Int }
(.) f g x = f (g x)
f x = x + 1
g x = x * 2
_old = (f . g) 3

main = print _old
