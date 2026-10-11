main = case Just 1 of { Just x -> let { f y = y; } in (f x,f True); Nothing -> (0,False); }
