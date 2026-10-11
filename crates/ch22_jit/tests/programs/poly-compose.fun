{ _old = (let { compose = (\f -> (\g -> (\x -> (f (g x))))); } in (((compose (\x -> (x + 1))) (\x -> (x * 2))) 10)); main = print _old; }
