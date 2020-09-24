module Allergies (Allergen (..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Eq, Show, Enum)

allergies :: Int -> [Allergen]
allergies score = filter (flip isAllergicTo score) [Eggs ..]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = toScore allergen .&. score /= 0

toScore :: Allergen -> Int
toScore Eggs = 1
toScore Peanuts = 2
toScore Shellfish = 4
toScore Strawberries = 8
toScore Tomatoes = 16
toScore Chocolate = 32
toScore Pollen = 64
toScore Cats = 128
