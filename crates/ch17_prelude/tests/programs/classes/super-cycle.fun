class B a => A a where
  a :: a -> Int
class A a => B a where
  b :: a -> Int
main = 1
