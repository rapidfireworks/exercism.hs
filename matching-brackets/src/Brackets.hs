module Brackets (arePaired) where

import Data.Foldable (foldl')

arePaired :: String -> Bool
arePaired xs = null $ foldl' go [] [x | x <- xs, isBracket x]
  where
    go ('[' : ys) ']' = ys
    go ('{' : ys) '}' = ys
    go ('(' : ys) ')' = ys
    go ys y = y : ys

isBracket :: Char -> Bool
isBracket '[' = True
isBracket ']' = True
isBracket '{' = True
isBracket '}' = True
isBracket '(' = True
isBracket ')' = True
isBracket _ = False
