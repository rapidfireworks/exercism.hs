module IsbnVerifier (isbn) where

import Data.Char (digitToInt)
import Data.Function ((&))
import Text.Regex.TDFA ((=~))

isbn :: String -> Bool
isbn = validate . parse

parse :: String -> [Int]
parse xs =
  concat groups
    & map readChar
  where
    regex = "\\`([[:digit:]])-?([[:digit:]]{3})-?([[:digit:]]{5})-?([[:digit:]X])\\'"
    (_, _, _, groups) = xs =~ regex :: (String, String, String, [String])
    readChar 'X' = 10
    readChar x = digitToInt x

validate :: [Int] -> Bool
validate xs@([_, _, _, _, _, _, _, _, _, _]) =
  zipWith (*) [10, 9 .. 1] xs
    & sum
    & (\x -> mod x 11 == 0)
validate _ = False

