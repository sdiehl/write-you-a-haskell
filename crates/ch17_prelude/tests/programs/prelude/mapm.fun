main = do { mapM (\n -> pure (n+1)) [1,2] >>= print }
