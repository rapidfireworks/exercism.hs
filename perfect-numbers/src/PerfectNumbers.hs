module PerfectNumbers (classify, Classification(..)) where

import qualified Data.Foldable as Foldable

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1
  = Nothing
classify n
  | result > n
  = Just Abundant
  | result == n
  = Just Perfect
  | otherwise
  = Just Deficient
  where result = aliquotSum n

aliquotSum :: Int -> Int
aliquotSum n
  | divMod n truncSqrt == (truncSqrt,  0)
  = sumOfDivisors - truncSqrt
  | otherwise
  = sumOfDivisors
  where
    truncSqrt = (truncate . sqrt . fromIntegral) n
    divisors = [x | x <- [2..truncSqrt], rem n x == 0]
    sumOfDivisors = 1 + Foldable.foldl' (\x y -> x + y + div n y) 0 divisors
