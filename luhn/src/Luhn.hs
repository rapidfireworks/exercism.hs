module Luhn (isValid) where

import Text.Read (readMaybe)

isValid :: String -> Bool
isValid n = case (luhnSum n) of
  Just x -> rem x 10 == 0
  Nothing -> False

luhnSum :: String -> Maybe Int
luhnSum digits = do
  parsed <- parse digits
  case parsed of
    xs@(_ : _ : _) -> return $ sum [clamp x | x <- zipWith (*) (cycle [1, 2]) $ reverse xs]
    _ -> Nothing
  where
    clamp x = if x < 10 then x else x - 9

parse :: String -> Maybe [Int]
parse text = do
  digits <- mapM readChar text
  return [digit | Right digit <- digits]

readChar :: Char -> Maybe (Either Char Int)
readChar letter = case (letter, readMaybe [letter]) of
  (' ', _) -> Just (Left ' ')
  (_, Just digit) -> Just (Right digit)
  (_, Nothing) -> Nothing
