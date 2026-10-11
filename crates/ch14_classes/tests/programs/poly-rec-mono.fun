{ main = (let { f = (\x -> (if True then (f 1) else (f True))); } in (f 0)); }
