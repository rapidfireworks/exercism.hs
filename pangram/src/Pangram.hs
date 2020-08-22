module Pangram (isPangram) where

import qualified Data.Char as Char
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram = (hasLetterSet (Set.fromList ['a'..'z'])) . (map Char.toLower)

hasLetterSet :: Set.Set Char -> String -> Bool
hasLetterSet set text
  = set == Set.fromList [x | x <- text, isAlphabet x]
  where isAlphabet x = Char.isAsciiLower x || Char.isAsciiUpper x
