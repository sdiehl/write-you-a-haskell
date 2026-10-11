{ _old = (let { f = (\f -> (f + 1)); } in (f 41)); main = print _old; }
