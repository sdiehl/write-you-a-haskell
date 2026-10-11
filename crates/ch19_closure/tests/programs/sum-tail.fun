{ _old = (let { sum = (\a -> (\n -> (if (n == 0) then a else ((sum (a + n)) (n - 1))))); } in ((sum 0) 100)); main = print _old; }
