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

data BST a = BST (Maybe (a, BST a, BST a)) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (BST (Just (_, x, _))) = Just x
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (BST (Just (_, _, x))) = Just x
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (BST (Just (x, _, _))) = Just x
bstValue _ = Nothing

empty :: BST a
empty = BST Nothing

fromList :: Ord a => [a] -> BST a
fromList xs = foldl' (flip insert) empty xs

insert :: Ord a => a -> BST a -> BST a
insert x (BST (Just (y, lhs, rhs)))
  | x > y = BST (Just (y, lhs, insert x rhs))
  | otherwise = BST (Just (y, insert x lhs, rhs))
insert x _ = singleton x

singleton :: a -> BST a
singleton x = BST (Just (x, empty, empty))

toList :: BST a -> [a]
toList (BST (Just (x, lhs, rhs))) = toList lhs ++ x : toList rhs
toList _ = []
