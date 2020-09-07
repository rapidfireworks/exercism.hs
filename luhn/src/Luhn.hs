module Luhn (isValid) where

import Data.Char (isSpace)
import Text.Read (readMaybe)

isValid :: String -> Bool
isValid n = case (luhnSum n) of
  Just x -> rem x 10 == 0
  Nothing -> False

luhnSum :: String -> Maybe Int
luhnSum text = do
  digits <- mapM readChar $ filter (not . isSpace) text
  case digits of
    -- NOTE: length digits > 1
    (_ : _ : _) -> return $ dotProduct digits
    _ -> Nothing
  where
    readChar letter = readMaybe [letter]
    luhnProduct x y = if p < 10 then p else p - 9 where p = x * y
    dotProduct = sum . zipWith luhnProduct (cycle [1, 2]) . reverse
