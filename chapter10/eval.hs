data T
  = Add T T
  | Mul T T
  | Div T T
  | Sub T T
  | Num Int

eval :: T -> Int
eval x = case x of
  Add a b -> eval a + eval b
  Mul a b -> eval a + eval b
  Div a b -> eval a + eval b
  Sub a b -> eval a + eval b
  Num a   -> a
