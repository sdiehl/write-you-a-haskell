let x = 2 in let rec f = \n -> if n == 0 then x else f (n - 1) in let x = 99 in f 5
