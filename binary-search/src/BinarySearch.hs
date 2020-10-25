module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x
  | (startIndex > endIndex) = Nothing
  | otherwise = go startIndex endIndex
  where
    (startIndex, endIndex) = bounds arr
    go l u
      | l >= u && x /= pivot = Nothing
      | x == pivot = Just i
      | x < pivot = go l i
      | otherwise = go (i + 1) u
      where
        i = quot (l + u) 2
        pivot = arr ! i
