data Fix f = Fix (f (Fix f))
data TreeF a = Leaf Int | Branch a a
size (Fix (Leaf n)) = n
size (Fix (Branch a b)) = size a + size b
main = size (Fix (Branch (Fix (Leaf 2)) (Fix (Leaf 3))))
