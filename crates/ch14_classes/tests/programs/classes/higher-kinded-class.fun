class Map f where
  mapIt :: (a -> b) -> f a -> f b
instance Map Maybe where
  mapIt f Nothing = Nothing
  mapIt f (Just x) = Just (f x)
main = mapIt (\x -> x + 1) (Just 41)
