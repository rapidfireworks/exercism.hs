module Yacht (yacht, Category (..)) where

import Data.Foldable (foldl')
import Data.List as List (sort, sortOn)
import Data.Map (Map)
import Data.Map.Strict as Map (empty, insertWith, toList)

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

yacht :: Category -> [Int] -> Int
yacht _ [] = 0
yacht _ [_] = 0
yacht _ [_, _] = 0
yacht _ [_, _, _] = 0
yacht _ [_, _, _, _] = 0
yacht _ (_ : _ : _ : _ : _ : _ : _) = 0
yacht FullHouse dies = case sortedHistogram dies of
  [(x, 2), (y, 3)] -> x * 2 + y * 3
  _ -> 0
yacht FourOfAKind dies = case sortedHistogram dies of
  [(_, 1), (x, 4)] -> x * 4
  [(x, 5)] -> x * 4
  _ -> 0
yacht LittleStraight dies = case List.sort dies of
  [1, 2, 3, 4, 5] -> 30
  _ -> 0
yacht BigStraight dies = case List.sort dies of
  [2, 3, 4, 5, 6] -> 30
  _ -> 0
yacht Yacht dies@(die : _)
  | all (die ==) dies = die * 10
  | otherwise = 0
yacht category dies = sum [die | die <- dies, isValid category die]
  where
    isValid Ones 1 = True
    isValid Twos 2 = True
    isValid Threes 3 = True
    isValid Fours 4 = True
    isValid Fives 5 = True
    isValid Sixes 6 = True
    isValid Choice _ = True
    isValid _ _ = False

histogram :: (Foldable t, Ord k) => t k -> Map k Integer
histogram = foldl' count Map.empty
  where
    count acc die = Map.insertWith (+) die 1 acc

sortedHistogram :: [Int] -> [(Int, Integer)]
sortedHistogram = List.sortOn snd . Map.toList . histogram
