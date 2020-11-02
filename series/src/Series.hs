module Series (slices) where

import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs = do
  Just slice <- takeWhile (not . null) result
  return slice
  where
    result = takeExact n <$> tails [read [x] | x <- xs]

takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _ [] = Nothing
takeExact n (x : xs) = (x :) <$> takeExact (pred n) xs
