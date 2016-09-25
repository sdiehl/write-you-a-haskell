-- Booleans
let T x y = x;
let F x y = y;

-- SKI combinators
let I x = x;
let K x y = x;
let S f g x = f x (g x);

let skk = S K K;

let Mu f = f (fix f);

-- Other combinators
let B x y z = x (y z);
let C x y z = x z y;
let W x y = x y y;

-- Integer arithmetic
let nsucc x = x + 1;
let npred x = x - 1;

-- Arithmetic
let succ n f x = f (n f x);

let zero  f x = x ;
let one   f x = f x ;
let two   f x = f (f x) ;
let three f x = f (f (f x)) ;
let four  f x = f (f (f (f x))) ;
let five  f x = f (f (f (f (f x)))) ;
let six   f x = f (f (f (f (f (f x))))) ;
let seven f x = f (f (f (f (f (f (f x)))))) ;
let eight f x = f (f (f (f (f (f (f (f x))))))) ;
let nine  f x = f (f (f (f (f (f (f (f (f x)))))))) ;
let ten   f x = f (f (f (f (f (f (f (f (f (f x))))))))) ;

let iszero n = n (\x -> F) T;
let plus m n f x = n f (m f x);
let mult m n f = m (n f);
let pow m n = n m;
let pred n f x = n (\g h -> h (g f)) (\u -> x) I;
let ack = \m -> m (\f n -> n f (f one)) succ;
let sub m n = (n pred) m;

-- Conversions

let unbool n = n True False;
let unchurch n = n (\i -> i + 1) 0;
let rec church n =
  if (n == 0)
  then zero
  else \f x -> f (church (n-1) f x);

-- Logic
let not p = p F T;
let and p q = p q F;
let or p q = p T q;
let cond p x y = p x y;
let xor p q = p (q F T) q;
let equ p q = not (xor p q);
let nand x y = cond x (not y) T;
let nor x y = cond x F (not y);

-- Tuples
let fst p = p T;
let snd p = p F;
let pair a b f = f a b;

-- Lists
let nil x = x;
let cons x y = pair F (pair x y);
let null z = z T;
let head z = fst (snd z);
let tail z = snd (snd z);
let indx xs n = head (n tail xs);

let fact = fix (\fact -> \n -> if (n == 0) then 1 else (n * (fact (n-1))));

let rec fib n =
  if (n == 0)
  then 0
  else if (n==1)
  then 1
  else ((fib (n-1)) + (fib (n-2)));

-- Functions
let const x y = x;
let compose f g = \x -> f (g x);
let twice f x = f (f x);
let on g f = \x y -> g (f x) (f y);
let ap f x = f (f x);

-- Let Polymorphism
let poly = I (I I) (I 3);
let self = (\x -> x) (\x -> x);
let innerlet = \x -> (let y = \z -> z in y);
let innerletrec = \x -> (let rec y = \z -> z in y);

-- Issue #72
let f = let add = \a b -> a + b in add;

-- Issue #82
let y = \y -> (let f = \x -> if x then True else False in const (f y) y);
let id x = x;
let foo x = let y = id x in y + 1;

-- Fresh variables
let wtf = \a b c d e e' f g h i j k l m n o o' o'' o''' p q r r' s t u u' v w x y z ->
    q u i c k b r o w n f o' x j u' m p s o'' v e r' t h e' l a z y d o''' g;

-- if-then-else
let notbool x = if x then False else True;
let eqzero x = if (x == 0) then True else False;

let rec until p f x =
  if (p x)
  then x
  else (until p f (f x));
