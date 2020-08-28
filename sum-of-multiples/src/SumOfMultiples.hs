module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [1..pred limit], isMultipleOfAny x]
  where
    isMultiple dividend divisor
      | divisor > 0 = rem dividend divisor == 0
      | otherwise = False
    isMultipleOfAny x = any (isMultiple x) factors
