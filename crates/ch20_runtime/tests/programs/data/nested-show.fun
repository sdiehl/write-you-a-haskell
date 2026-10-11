_old = (Just (Left 3), [Just (-3), Nothing], ['a','b'])

main = print (_old :: (Maybe (Either Int Int), [Maybe Int], [Char]))
