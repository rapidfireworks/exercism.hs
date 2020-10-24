module Prime (nth) where

import Control.Lens (element, (^?))

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = [x | x <- candidate, isPrime x] ^? element (pred n)
  where
    candidate = 2 : 3 : (scanl1 (+) (5 : cycle [2, 4]))

isPrime :: Integral a => a -> Bool
isPrime x = x > 1 && all (notFactorOf x) [2 .. truncSqrt x]
  where
    truncSqrt = truncate . (sqrt :: Double -> Double) . fromIntegral
    notFactorOf dividend divisor = rem dividend divisor /= 0
