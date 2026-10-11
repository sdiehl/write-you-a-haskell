let rec sum : Int -> Int -> Int = \(a : Int) -> \(n : Int) -> if n == 0 then a else sum (a + n) (n - 1) in sum 0 100
