keep :: Maybe a -> Maybe a
keep x = x
_old = (keep (Just 1),keep (Just True))

main = print _old
