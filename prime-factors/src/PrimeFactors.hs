module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = _primeFactor n 2
  where
    _primeFactor k x
      | k < x = []
      | isPrime x && r == 0 = x : _primeFactor q x
      | otherwise = _primeFactor k (x + 1)
      where
        (q, r) = quotRem k x

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
isPrime n = all notFactor $ takeWhile (truncSqrt n >) candid
  where
    candid = scanl1 (+) (2 : 1 : 2 : cycle [2, 4])
    notFactor = (0 /=) . rem n

truncSqrt :: Integer -> Integer
truncSqrt = (truncate :: Double -> Integer) . sqrt . fromIntegral
