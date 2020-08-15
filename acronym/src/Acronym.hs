module Acronym (abbreviate) where

import qualified Data.Char

abbreviate :: String -> String
abbreviate xs =
  let
    abbr "" = ""
    abbr (_:"") = ""
    abbr (first:rest@(second:_))
      | isAbbrSeparator first && Data.Char.isAlpha second
      = Data.Char.toUpper second:abbr rest
      | Data.Char.isLower first && Data.Char.isUpper second
      = second:abbr rest
      | otherwise
      = abbr rest

    isAbbrSeparator '-' = True
    isAbbrSeparator '_' = True
    isAbbrSeparator letter = Data.Char.isSpace letter
  in
    abbr (' ':xs)
