module Hamming (distance) where

import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable

distance :: String -> String -> Maybe Int
distance xs = (Foldable.foldl' (Applicative.liftA2 (+)) (Just 0)) . (takeUntil (Nothing ==)) . (map diffScore) . (maybeZip xs)

maybeZip :: [a] -> [a] -> [(Maybe a, Maybe a)]
maybeZip [] [] = []
maybeZip [] ys = [(Nothing, Just y) | y <- ys]
maybeZip xs [] = [(Just x, Nothing) | x <- xs]
maybeZip (x:xs) (y:ys) = (Just x, Just y):maybeZip xs ys

diffScore :: Eq a => (Maybe a, Maybe a) -> Maybe Int
diffScore (Nothing, _) = Nothing
diffScore (_, Nothing) = Nothing
diffScore (lhs, rhs)
  | lhs == rhs
  = Just 0
  | otherwise
  = Just 1

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil _ [x] = [x]
takeUntil f (x:xs)
  | f x
  = [x]
  | otherwise
  = x:takeUntil f xs
