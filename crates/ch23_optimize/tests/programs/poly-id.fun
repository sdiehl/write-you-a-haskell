{ _old = (let { id = (\x -> x); } in (let { a = (id 1); } in (id True))); main = print _old; }
