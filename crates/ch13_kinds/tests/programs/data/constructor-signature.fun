keep :: Maybe a -> Maybe a
keep x = x
main = (keep (Just 1),keep (Just True))
