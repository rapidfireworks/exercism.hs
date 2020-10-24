module LinkedList (LinkedList, datum, fromList, isNil, new, next, nil, reverseLinkedList, toList) where

import Data.Foldable (foldr')
import GHC.OldList (unfoldr)

data LinkedList a = Node a (LinkedList a) | None deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Node d _) = d

fromList :: [a] -> LinkedList a
fromList xs = foldr' Node None xs

isNil :: LinkedList a -> Bool
isNil None = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Node

next :: LinkedList a -> LinkedList a
next (Node _ n) = n

nil :: LinkedList a
nil = None

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = _reverse linkedList None
  where
    _reverse None result = result
    _reverse (Node d n) result = _reverse n (Node d result)

toList :: LinkedList a -> [a]
toList linkedList = unfoldr unwrap linkedList
  where
    unwrap None = Nothing
    unwrap (Node d n) = Just (d, n)
