{ _old = (let { x = 1; } in (let { f = (\y -> (x + y)); } in (let { x = 100; } in (f 2)))); main = print _old; }
