{ _old = (let { x = 1; } in ((let { x = 2; } in x) + x)); main = print _old; }
