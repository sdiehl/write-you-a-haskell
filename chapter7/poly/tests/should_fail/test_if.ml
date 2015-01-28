let f = (\x -> if x then x else (x+1));
let g = (\x -> if x then 1 else (x+1));
let h = (\x -> if x then (x+1) else 1);
let j = (\x -> fix (x 0));
