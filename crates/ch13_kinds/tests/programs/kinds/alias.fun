type Number = Int
type Pair a = (a,a)
sum :: Pair Number -> Number
sum (x,y) = x+y
main = sum (20,22)
