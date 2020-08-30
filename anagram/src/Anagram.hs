module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map (fromListWith)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    (xsLower, xsFreq) = lowerFreq xs
    isAnagram ys = xsLower /= ysLower && xsFreq == ysFreq
      where
        (ysLower, ysFreq) = lowerFreq ys

lowerFreq :: String -> (String, Map Char Int)
lowerFreq xs = (lower, frequency)
  where
    lower = map toLower xs
    frequency = Map.fromListWith (+) [(x, 1) | x <- lower]
