{ main = (let { id = (\x -> x); } in (let { id = (\x -> (x + 1)); } in (id 41))); }
