{ _old = (let { f = (\n -> (let { g = (\m -> (if (m == 0) then n else (g (m - 1)))); } in (g 3))); } in (f 7)); main = print _old; }
