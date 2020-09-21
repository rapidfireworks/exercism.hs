module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = take x $ iterate next [1]
  where
    next row = zipWith (+) (0 : row) (row ++ [0])
