module RotationalCipher (rotate) where

import Data.Char

rotate :: Int -> String -> String
rotate offset = map (chr . go)
  where
    go x
      | isUpper x = upA + mod (ord x - upA + offset) period
      | isLower x = loA + mod (ord x - loA + offset) period
      | otherwise = ord x
    upA = ord 'A'
    loA = ord 'a'
    period = ord 'z' - loA + 1
