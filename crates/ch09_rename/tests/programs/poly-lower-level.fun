{ main = (\x -> (let { y = (\z -> (x z)); } in (let { a = (y 1); } in (y True)))); }
