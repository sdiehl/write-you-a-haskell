{ _old = ((let { f = (\n -> (if (n == 0) then 1 else (n * (f (n - 1))))); } in f) 5); main = print _old; }
