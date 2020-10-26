module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.Function ((&))

decode :: String -> String
decode cipherText =
  sanitize cipherText
    & map convert

encode :: String -> String
encode plainText =
  sanitize plainText
    & map convert
    & space

sanitize :: [Char] -> [Char]
sanitize xs = [toLower x | x <- xs, isAlphaNum x]

convert :: Char -> Char
convert x
  | isAlpha x = toEnum result
  | otherwise = x
  where
    result = fromEnum 'z' - fromEnum x + fromEnum 'a'

space :: [Char] -> [Char]
space xs = drop 1 $ do
  (x, s) <- zip xs $ cycle [Just ' ', Nothing, Nothing, Nothing, Nothing]
  Just y <- [s, Just x]
  return y
