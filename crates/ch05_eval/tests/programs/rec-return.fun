let rec f : Int -> Int -> Int = \(n : Int) -> if n == 0 then (\(x : Int) -> x) else f (n - 1) in f 5 42
