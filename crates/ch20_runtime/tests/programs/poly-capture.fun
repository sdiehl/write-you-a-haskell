{ _old = ((\x -> (let { f = (\y -> x); } in (let { a = (f 1); } in (f True)))) 42); main = print _old; }
