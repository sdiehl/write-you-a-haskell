type Number = Int
type Pair a = (a,a)
sum :: Pair Number -> Number
sum (x,y) = x+y
_old = sum (20,22)

main = print _old
