{ _old = (let { f = (\n -> n); } in (f 1)); main = print _old; }
