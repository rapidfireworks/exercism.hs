module Bob (responseFor) where

import Data.Char (isLetter, isSpace, isUpper)

responseFor :: String -> String
responseFor xs
  | silent xs = "Fine. Be that way!"
  | yelling xs && asking xs = "Calm down, I know what I'm doing!"
  | asking xs = "Sure."
  | yelling xs = "Whoa, chill out!"
  | otherwise = "Whatever."

yelling xs = length letters > 0 && all isUpper letters
  where
    letters = [x | x <- xs, isLetter x]

asking "" = False
asking ('?' : xs)
  | all isSpace xs = True
asking (_ : xs) = asking xs

silent xs = all isSpace xs
