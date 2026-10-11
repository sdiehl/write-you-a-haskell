type Pair a = (a,a)
data Bad f = Bad (f Int)
f :: Bad Pair -> Int
f x = 1
main = 1
