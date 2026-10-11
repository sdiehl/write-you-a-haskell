type Id a = a
f :: Id (Id Int) -> Int
f x = x
main = f 42
