module Pangram (isPangram) where

import qualified Data.Char as Char
import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram text = all inText ['a' .. 'z']
  where
    set = Set.fromList [Char.toLower x | x <- text]
    inText letter = Set.member letter set
