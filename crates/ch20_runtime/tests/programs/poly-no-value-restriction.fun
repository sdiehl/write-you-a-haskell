{ _old = (let { id = ((\f -> f) (\x -> x)); } in (let { a = (id 1); } in (id True))); main = print _old; }
