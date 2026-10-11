class Area a where
  area :: a -> Double
data Shape = Circle Double | Rect Double Double
instance Area Shape where
  area (Circle r) = 3.0 * r * r
  area (Rect w h) = w * h
main = (area (Circle 1.0),area (Rect 2.0 3.0))
