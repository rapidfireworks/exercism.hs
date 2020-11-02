module Series (slices) where

import qualified Data.List.NonEmpty as NE (toList, unfoldr)

slices :: Int -> String -> [[Int]]
slices n xs = do
  Just slice <- takeWhile (not . null) result
  return slice
  where
    result = takeExact n <$> heads [read [x] | x <- xs]

heads :: [a] -> [[a]]
heads = NE.toList . NE.unfoldr go
  where
    go [] = ([], Nothing)
    go xs@(_ : tl) = (xs, Just tl)

takeExact :: Int -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _ [] = Nothing
takeExact n (x : xs) = (x :) <$> takeExact (pred n) xs
