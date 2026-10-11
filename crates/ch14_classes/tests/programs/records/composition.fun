data P = P { x :: Int, y :: Int }
(.) f g x = f (g x)
f x = x + 1
g x = x * 2
main = (f . g) 3
