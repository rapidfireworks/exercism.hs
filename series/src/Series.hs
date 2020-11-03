module Series (slices) where

import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs = take n <$> zipWith const xss (drop n xss)
  where
    xss = tails $ read <$> pure <$> xs
