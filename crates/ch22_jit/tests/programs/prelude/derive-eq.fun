data T a = A a | B deriving (Eq, Show)
main = print (A 1 == A 1, A 1 == B, (B :: T Int) == B)
