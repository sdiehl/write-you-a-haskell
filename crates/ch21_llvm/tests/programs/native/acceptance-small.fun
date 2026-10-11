data Shape = Circle Double | Rect Double Double
  deriving (Show)
data Point = Point { x :: Double, y :: Double }
  deriving (Eq, Show)
class Area a where
  area :: a -> Double
instance Area Shape where
  area (Circle r) = 3.0 * r * r
  area (Rect w h) = w * h
norm :: Point -> Double
norm p = sqrt (p.x * p.x + p.y * p.y)
sumTo :: Int -> Int
sumTo n = go 0 n
  where
    go acc 0 = acc
    go acc k = go (acc + k) (k - 1)
main :: IO ()
main = do
  let p = Point { x = 3.0, y = 4.0 }
  print (norm p)
  mapM_ (print . area) [Circle 1.0, Rect 2.0 3.0]
  print (map (.x) [p, p { x = 10.0 }])
  print (p == p { y = 4.0 })
  print (sumTo 1000)
