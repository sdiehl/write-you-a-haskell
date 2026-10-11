{ sum n = if n == 0 then 0 else n + sum (n - 1); _old = sum 100; main = print _old; }
