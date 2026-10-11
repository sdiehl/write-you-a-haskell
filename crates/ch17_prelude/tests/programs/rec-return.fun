{ _old = (let { f = (\n -> (if (n == 0) then (\x -> x) else (f (n - 1)))); } in ((f 5) 42)); main = print _old; }
