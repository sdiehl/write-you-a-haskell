{ main = (let { k = (\x -> (\y -> x)); } in ((k 42) True)); }
