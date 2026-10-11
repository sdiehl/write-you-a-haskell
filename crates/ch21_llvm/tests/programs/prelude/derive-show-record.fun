data P a = P {x :: a, y :: Int} deriving (Eq, Ord, Show)
main = print (Just (P {y=2,x=Left 1} :: P (Either Int Bool)))
