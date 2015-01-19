infixl 3 ($);
infixr 4 (#);

infix 4 (.);

prefix 10 (-);
postfix 10 (!);

let a = y in a $ a $ (-a)!;
let b = y in a # a # a $ b;
let c = y in a # a # a # b;
