{ twice f x = f (f x); _old = twice (\x -> x + 1) 40; main = print _old; }
