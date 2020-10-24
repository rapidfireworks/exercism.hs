module Triangle (TriangleType (..), triangleType) where

import Data.List as List (sort)

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = classify $ List.sort [a, b, c]
  where
    classify [x, y, z]
      | x + y <= z = Illegal
      | x == z = Equilateral
      | x == y || y == z = Isosceles
    classify _ = Scalene
