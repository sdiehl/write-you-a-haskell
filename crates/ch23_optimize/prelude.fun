infixr 9 .
infixl 7 *, /, `div`, `mod`, `quot`, `rem`
infixl 6 +, -
infixr 6 <>
infixr 5 :, ++
infix 4 ==, /=, <, <=, >, >=
infixr 3 &&
infixr 2 ||
infixl 4 <$>, <*>
infixl 1 >>=, >>
infixr 0 $

data Bool = False | True
data Ordering = LT | EQ | GT
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data [] a = [] | (:) a [a]
data () = ()
data (,) a b = (,) a b
data (,,) a b c = (,,) a b c

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = if x == y then False else True
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a
  x < y = case compare x y of { LT -> True; _ -> False }
  x <= y = case compare x y of { GT -> False; _ -> True }
  x > y = y < x
  x >= y = y <= x
  max x y = if x <= y then y else x
  min x y = if x <= y then x else y
class Show a where
  show :: a -> String
  _showsPrec :: Int -> a -> String
  _showsPrec p x = if p > 10 then _showArg (show x) else show x
class Num a where
  (+), (-), (*) :: a -> a -> a
  negate, abs, signum :: a -> a
class Num a => Fractional a where
  (/) :: a -> a -> a
class Semigroup a where
  (<>) :: a -> a -> a
class Semigroup a => Monoid a where
  mempty :: a

instance Eq Int where
  x == y = prim_intEq x y
instance Ord Int where
  compare x y = if prim_intEq x y then EQ else if prim_intLt x y then LT else GT
instance Show Int where
  show x = prim_showInt x
instance Num Int where
  x + y = prim_intAdd x y
  x - y = prim_intSub x y
  x * y = prim_intMul x y
  negate x = prim_intNeg x
  abs x = if prim_intLt x 0 then prim_intNeg x else x
  signum x = if prim_intEq x 0 then 0 else if prim_intLt x 0 then -1 else 1
instance Eq Double where
  x == y = prim_dblEq x y
instance Ord Double where
  compare x y = if prim_dblEq x y then EQ else if prim_dblLt x y then LT else GT
  x < y = prim_dblLt x y
  x <= y = prim_dblLe x y
  x > y = prim_dblLt y x
  x >= y = prim_dblLe y x
instance Show Double where
  show x = prim_showDouble x
instance Num Double where
  x + y = prim_dblAdd x y
  x - y = prim_dblSub x y
  x * y = prim_dblMul x y
  negate x = prim_dblNeg x
  abs x = if prim_dblEq x 0.0 then 0.0 else if prim_dblLt x 0.0 then prim_dblNeg x else x
  signum x = if prim_dblEq x 0.0 then 0.0 else if prim_dblLt x 0.0 then -1.0 else if prim_dblLe 0.0 x then 1.0 else x
instance Fractional Double where
  x / y = prim_dblDiv x y
instance Eq Char where
  x == y = prim_chrEq x y
instance Ord Char where
  compare x y = if prim_chrEq x y then EQ else if prim_chrLt x y then LT else GT
instance Show Char where
  show x = prim_showChar x
instance Eq String where
  x == y = prim_strEq x y
instance Ord String where
  compare x y = case prim_strCompare x y of { -1 -> LT; 0 -> EQ; _ -> GT }
instance Show String where
  show x = prim_showString x
instance Semigroup String where
  x <> y = prim_strAppend x y
instance Monoid String where
  mempty = ""
instance Eq Bool where
  False == False = True
  True == True = True
  _ == _ = False
instance Ord Bool where
  compare False False = EQ
  compare False True = LT
  compare True False = GT
  compare True True = EQ
instance Show Bool where
  show False = "False"
  show True = "True"
instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False
instance Ord Ordering where
  compare LT LT = EQ
  compare LT _ = LT
  compare EQ LT = GT
  compare EQ EQ = EQ
  compare EQ GT = LT
  compare GT GT = EQ
  compare GT _ = GT
instance Show Ordering where
  show LT = "LT"
  show EQ = "EQ"
  show GT = "GT"
instance Eq () where
  () == () = True
instance Ord () where
  compare () () = EQ
instance Show () where
  show () = "()"
instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _ == _ = False
instance Ord a => Ord [a] where
  compare [] [] = EQ
  compare [] _ = LT
  compare _ [] = GT
  compare (x:xs) (y:ys) = case compare x y of { EQ -> compare xs ys; order -> order }
instance Show a => Show [a] where
  show xs = "[" <> _showItems xs <> "]"
instance Semigroup [a] where
  [] <> ys = ys
  (x:xs) <> ys = x : (xs <> ys)
instance Monoid [a] where
  mempty = []
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  (Just x) == (Just y) = x == y
  _ == _ = False
instance Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare Nothing _ = LT
  compare _ Nothing = GT
  compare (Just x) (Just y) = compare x y
instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " <> _showArg (show x)
instance (Eq a, Eq b) => Eq (Either a b) where
  (Left x) == (Left y) = x == y
  (Right x) == (Right y) = x == y
  _ == _ = False
instance (Ord a, Ord b) => Ord (Either a b) where
  compare (Left x) (Left y) = compare x y
  compare (Left x) (Right y) = LT
  compare (Right x) (Left y) = GT
  compare (Right x) (Right y) = compare x y
instance (Show a, Show b) => Show (Either a b) where
  show (Left x) = "Left " <> _showArg (show x)
  show (Right x) = "Right " <> _showArg (show x)
instance (Eq a, Eq b) => Eq (a,b) where
  (a,b) == (c,d) = a == c && b == d
instance (Ord a, Ord b) => Ord (a,b) where
  compare (a,b) (c,d) = case compare a c of { EQ -> compare b d; order -> order }
instance (Show a, Show b) => Show (a,b) where
  show (a,b) = "(" <> show a <> "," <> show b <> ")"
instance (Eq a, Eq b, Eq c) => Eq (a,b,c) where
  (a,b,c) == (d,e,f) = a == d && b == e && c == f
instance (Ord a, Ord b, Ord c) => Ord (a,b,c) where
  compare (a,b,c) (d,e,f) = case compare a d of { EQ -> case compare b e of { EQ -> compare c f; order -> order }; order -> order }
instance (Show a, Show b, Show c) => Show (a,b,c) where
  show (a,b,c) = "(" <> show a <> "," <> show b <> "," <> show c <> ")"

(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)
_showItems [] = ""
_showItems [x] = show x
_showItems (x:xs) = show x <> "," <> _showItems xs
_showArg s = case prim_strUnpack s of
  '(':_ -> s
  '[':_ -> s
  '"':_ -> s
  '\'':_ -> s
  '-':_ -> "(" <> s <> ")"
  chars -> if _hasSpace chars then "(" <> s <> ")" else s
_hasSpace [] = False
_hasSpace (' ':xs) = True
_hasSpace (x:xs) = _hasSpace xs

div x y = prim_intDiv x y
mod x y = prim_intMod x y
quot x y = prim_intQuot x y
rem x y = prim_intRem x y
toDouble x = prim_intToDbl x
floor x = prim_dblFloor x
round x = prim_dblRound x
truncate x = prim_dblTruncate x
sqrt x = prim_dblSqrt x
exp x = prim_dblExp x
log x = prim_dblLog x
sin x = prim_dblSin x
cos x = prim_dblCos x
(**) x y = prim_dblPow x y
ord x = prim_chrOrd x
chr x = prim_chrChr x
pack x = prim_strPack x
unpack x = prim_strUnpack x
readInt x = prim_readInt x
error s = prim_error s
trace :: String -> a -> a
trace s x = prim_trace s x

class Functor f where
  fmap :: (a -> b) -> f a -> f b
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  m >> n = m >>= (\_ -> n)

returnIO :: a -> IO a
returnIO x = \_ -> x
bindIO :: IO a -> (a -> IO b) -> IO b
bindIO m k = \u -> k (m u) u
putStr :: String -> IO ()
putStr s = \_ -> prim_putStr s
getLine :: IO String
getLine = \u -> prim_getLine u
exitWith :: Int -> IO a
exitWith n = \_ -> prim_exit n

instance Functor IO where
  fmap f m = bindIO m (\x -> returnIO (f x))
instance Applicative IO where
  pure = returnIO
  fs <*> xs = bindIO fs (\f -> bindIO xs (\x -> returnIO (f x)))
instance Monad IO where
  (>>=) = bindIO
instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)
instance Applicative Maybe where
  pure = Just
  Nothing <*> xs = Nothing
  (Just f) <*> xs = fmap f xs
instance Monad Maybe where
  Nothing >>= f = Nothing
  (Just x) >>= f = f x
instance Functor (Either e) where
  fmap f (Left e) = Left e
  fmap f (Right x) = Right (f x)
instance Applicative (Either e) where
  pure = Right
  (Left e) <*> xs = Left e
  (Right f) <*> xs = fmap f xs
instance Monad (Either e) where
  (Left e) >>= f = Left e
  (Right x) >>= f = f x
instance Functor [] where
  fmap f [] = []
  fmap f (x:xs) = f x : fmap f xs
instance Applicative [] where
  pure x = [x]
  [] <*> xs = []
  (f:fs) <*> xs = fmap f xs <> (fs <*> xs)
instance Monad [] where
  [] >>= f = []
  (x:xs) >>= f = f x <> (xs >>= f)

(<$>) f x = fmap f x
putStrLn s = putStr (s <> "\n")
print x = putStrLn (show x)
mapM_ f [] = pure ()
mapM_ f (x:xs) = f x >> mapM_ f xs
forM_ xs f = mapM_ f xs
mapM f [] = pure []
mapM f (x:xs) = f x >>= (\y -> mapM f xs >>= (\ys -> pure (y:ys)))
sequence_ [] = pure ()
sequence_ (m:ms) = m >> sequence_ ms
when b m = if b then m else pure ()
unless b m = when (if b then False else True) m

_showParen b s = if b then "(" <> s <> ")" else s
id x = x
const x y = x
flip f x y = f y x
(.) f g x = f (g x)
($) f x = f x
fst (x,y) = x
snd (x,y) = y
curry f x y = f (x,y)
uncurry f (x,y) = f x y
otherwise = True
not b = if b then False else True
_undefined _ = error "undefined"
map f [] = []
map f (x:xs) = f x : map f xs
filter f [] = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
foldl' f z xs = foldl f z xs
length xs = foldl (\n _ -> n + 1) 0 xs
null [] = True
null (_:_) = False
head [] = error "head: empty list"
head (x:_) = x
tail [] = error "tail: empty list"
tail (_:xs) = xs
reverse xs = foldl (\ys x -> x:ys) [] xs
concat xs = foldr (++) [] xs
concatMap f xs = concat (map f xs)
sum xs = foldl (+) 0 xs
product xs = foldl (*) 1 xs
and [] = True
and (x:xs) = x && and xs
or [] = False
or (x:xs) = x || or xs
any p [] = False
any p (x:xs) = p x || any p xs
all p [] = True
all p (x:xs) = p x && all p xs
elem x xs = any (x ==) xs
lookup k [] = Nothing
lookup k ((x,y):xs) = if k == x then Just y else lookup k xs
zip xs ys = zipWith (,) xs ys
zipWith f [] ys = []
zipWith f (x:xs) [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
unzip xs = (map fst xs, map snd xs)
take n [] = []
take n (x:xs) = if n <= 0 then [] else x : take (n-1) xs
drop n [] = []
drop n xs@(x:rest) = if n <= 0 then xs else drop (n-1) rest
splitAt n xs = (take n xs, drop n xs)
takeWhile p [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []
dropWhile p [] = []
dropWhile p xs@(x:rest) = if p x then dropWhile p rest else xs
replicate n x = if n <= 0 then [] else x : replicate (n-1) x
enumFromTo :: Int -> Int -> [Int]
enumFromTo a b = if a > b then [] else if a == b then [a] else a : enumFromTo (a+1) b
iterateN n f x = if n <= 0 then [] else x : (if n == 1 then [] else iterateN (n-1) f (f x))
lines s = map pack (_lines (unpack s))
_lines [] = []
_lines xs = let { a = takeWhile (\c -> c /= '\n') xs; b = dropWhile (\c -> c /= '\n') xs; } in a : (case b of { [] -> []; (_:rest) -> _lines rest })
unlines ss = foldr (\s rest -> s <> "\n" <> rest) "" ss
words s = map pack (_words (unpack s))
_words xs = case dropWhile isSpace xs of { [] -> []; rest -> takeWhile (\c -> not (isSpace c)) rest : _words (dropWhile (\c -> not (isSpace c)) rest) }
unwords [] = ""
unwords [x] = x
unwords (x:xs) = x <> " " <> unwords xs
even n = n `rem` 2 == 0
odd n = not (even n)
gcd a b = if b == 0 then abs a else gcd b (a `rem` b)
maybe z f Nothing = z
maybe z f (Just x) = f x
either f g (Left x) = f x
either f g (Right x) = g x
