module Pangram (isPangram) where

import qualified Data.Char as Char
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram = (26 ==) . length . letterSet

letterSet :: String -> Set.Set Char
letterSet text
  = Set.fromList [Char.toLower x | x <- text, isAlphabet x]
  where isAlphabet x = Char.isAsciiLower x || Char.isAsciiUpper x
