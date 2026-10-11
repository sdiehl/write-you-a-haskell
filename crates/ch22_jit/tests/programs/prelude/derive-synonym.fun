type K a = Int
data T a = T (K a) deriving Eq
main = print ((T 1 :: T (Int -> Int)) == T 1)
