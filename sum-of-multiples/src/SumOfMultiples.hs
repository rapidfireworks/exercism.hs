module SumOfMultiples (sumOfMultiples) where

import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, union)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  factors
    & map (multiples (pred limit))
    & foldl Set.union Set.empty
    & sum

multiples :: Integer -> Integer -> Set Integer
multiples limit factor
  | factor > 0 = Set.fromList [factor, 2 * factor .. limit]
  | otherwise = Set.empty
