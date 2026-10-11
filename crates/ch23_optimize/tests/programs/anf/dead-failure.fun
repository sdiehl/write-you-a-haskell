main = let { x = error "strict unused let" :: Int; } in pure ()
