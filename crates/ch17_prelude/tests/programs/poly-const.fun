{ _old = (let { k = (\x -> (\y -> x)); } in ((k 42) True)); main = print _old; }
