module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)
import Data.Map (Map)
import Data.Map.Strict as Map (findWithDefault, fromList)

scoreLetter :: Char -> Integer
scoreLetter letter = Map.findWithDefault 0 letter preset

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter

preset :: Map Char Integer
preset = Map.fromList $ do
  (letters, score) <-
    ("AEIOULNRST", 1) :
    ("DG", 2) :
    ("BCMP", 3) :
    ("FHVWY", 4) :
    ("K", 5) :
    ("JX", 8) :
    ("QZ", 10) : []
  letter <- letters
  [(letter, score), (toLower letter, score)]
