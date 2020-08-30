module Anagram (anagramsFor) where

import Data.Char (toLower)
import qualified Data.List as List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    xsLower = map toLower xs
    xsSorted = List.sort xsLower
    isAnagram ys = xsLower /= ysLower && xsSorted == ysSorted
      where
        ysLower = map toLower ys
        ysSorted = List.sort ysLower
