type One a = Two a
type Two a = [a]
f :: One Int -> Int
f (x : xs) = x
_old = f [42]

main = print _old
