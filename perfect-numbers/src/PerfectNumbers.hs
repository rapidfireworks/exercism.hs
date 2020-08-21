module PerfectNumbers (classify, Classification(..)) where

import qualified Data.Foldable as Foldable

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1
  = Nothing
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
  = sumOfFactors - truncSqrt
  | otherwise
  = sumOfFactors
  where
    truncSqrt = (truncate . sqrt . fromIntegral) n
    factors = [x | x <- [2..truncSqrt], rem n x == 0]
    addFactorPair acc factor = acc + factor + div n factor
    sumOfFactors = Foldable.foldl' addFactorPair 1 factors
