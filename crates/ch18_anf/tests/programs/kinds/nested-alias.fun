type Id a = a
f :: Id (Id Int) -> Int
f x = x
_old = f 42

main = print _old
