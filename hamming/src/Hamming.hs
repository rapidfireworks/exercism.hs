module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = (Just . sum . (map diffScore)) (zip xs ys)
  where
    diffScore (lhs, rhs)
      | lhs == rhs = 0
      | otherwise = 1
