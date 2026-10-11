{ _old = (let { ignored = (div 1 0); } in 42); main = (\_ -> pure ()) _old; }
