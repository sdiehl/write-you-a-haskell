{ _old = (let { id = (\x -> x); } in (id (id 42))); main = print _old; }
