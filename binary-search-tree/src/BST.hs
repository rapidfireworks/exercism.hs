module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

import Data.Foldable (foldl')

data BST a = Empty | BST (a, BST a, BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (BST (_, x, _)) = Just x
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (BST (_, _, x)) = Just x
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (BST (x, _, _)) = Just x
bstValue _ = Nothing

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList xs = foldl' (flip insert) empty xs

insert :: Ord a => a -> BST a -> BST a
insert x (BST (y, lhs, rhs))
  | x > y = BST (y, lhs, insert x rhs)
  | otherwise = BST (y, insert x lhs, rhs)
insert x _ = singleton x

singleton :: a -> BST a
singleton x = BST (x, empty, empty)

toList :: BST a -> [a]
toList (BST (x, lhs, rhs)) = toList lhs ++ x : toList rhs
toList _ = []
