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

data BST a
  = Empty
  | Node {left :: (BST a), value :: a, right :: (BST a)}
  deriving (Eq, Show)

instance Foldable BST where
  foldMap _ Empty = mempty
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

maybeNode :: BST a -> Maybe (BST a)
maybeNode Empty = Nothing
maybeNode x = Just x

bstLeft :: BST a -> Maybe (BST a)
bstLeft = fmap left . maybeNode

bstRight :: BST a -> Maybe (BST a)
bstRight = fmap right . maybeNode

bstValue :: BST a -> Maybe a
bstValue = fmap value . maybeNode

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node l y r)
  | x > y = Node l y (insert x r)
  | otherwise = Node (insert x l) y r

singleton :: a -> BST a
singleton = flip (Node empty) empty

toList :: BST a -> [a]
toList = foldr (:) []
