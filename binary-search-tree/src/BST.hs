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

data BST a = Empty | BST {value :: a, left :: (BST a), right :: (BST a)} deriving (Eq, Show)

toMaybe :: BST a -> Maybe (BST a)
toMaybe Empty = Nothing
toMaybe x = Just x

bstLeft :: BST a -> Maybe (BST a)
bstLeft = fmap left . toMaybe

bstRight :: BST a -> Maybe (BST a)
bstRight = fmap right . toMaybe

bstValue :: BST a -> Maybe a
bstValue = fmap value . toMaybe

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList xs = foldl' (flip insert) Empty xs

insert :: Ord a => a -> BST a -> BST a
insert x (BST y lhs rhs)
  | x > y = BST y lhs (insert x rhs)
  | otherwise = BST y (insert x lhs) rhs
insert x _ = singleton x

singleton :: a -> BST a
singleton x = BST x Empty Empty

toList :: BST a -> [a]
toList (BST x lhs rhs) = toList lhs ++ x : toList rhs
toList _ = []
