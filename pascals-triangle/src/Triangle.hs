module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = triangle x [1]
  where
    triangle n row | n > 0 = row : triangle (n - 1) (next row)
    triangle _ _ = []

next :: [Integer] -> [Integer]
next row = zipWith (+) period (tail $ cycle period)
  where
    period = 0 : row
