type One a = Two a
type Two a = [a]
f :: One Int -> Int
f (x : xs) = x
main = f [42]
