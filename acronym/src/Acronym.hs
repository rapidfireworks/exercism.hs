module Acronym (abbreviate) where

import qualified Data.Char as Char

abbreviate :: String -> String
abbreviate xs =
  let
    isInitial prev curr =
      (isSeparator prev && Char.isAlpha curr) ||
      (Char.isLower prev && Char.isUpper curr)

    isSeparator '-' = True
    isSeparator '_' = True
    isSeparator letter = Char.isSpace letter
  in
    [Char.toUpper curr | (prev, curr) <- zip (' ':xs) xs, isInitial prev curr]
