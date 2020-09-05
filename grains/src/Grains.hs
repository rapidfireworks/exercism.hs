module Grains (square, total) where

import Data.Ix (inRange)
import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n
  | inRange (1, 64) n = Just (2 ^ pred n)
  | otherwise = Nothing

total :: Integer
total = sum [fromJust (square x) | x <- [1 .. 64]]
