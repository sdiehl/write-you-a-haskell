{ _old = (((\x -> (\b -> (if b then x else 0))) 42) True); main = print _old; }
