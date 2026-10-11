let rec f : Int -> Int = \(n : Int) -> let rec g : Int -> Int = \(m : Int) -> if m == 0 then n else g (m - 1) in g 3 in f 7
