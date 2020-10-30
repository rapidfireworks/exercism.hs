module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2 .. n]
  where
    sieve [] = []
    sieve (x : xs) = x : sieve (go xs x)
      where
        go [] _ = []
        go l@(y : ys) multiple
          | y < multiple = y : go ys multiple
          | y == multiple = go ys (multiple + x)
          | otherwise = go l (multiple + x)
