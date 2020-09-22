{-# LANGUAGE BangPatterns #-}

module ListOps (length, reverse, map, filter, foldr, foldl', (++), concat) where

import Prelude hiding (concat, filter, foldr, length, map, reverse, (++))

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !z [] = z
foldl' f !z (x : xs) = foldl' f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x : xs) = f x $ foldr f z xs

length :: [a] -> Int
length xs = foldl' (flip $ const succ) 0 xs

reverse :: [a] -> [a]
reverse xs = _reverse xs []
  where
    _reverse [] result = result
    _reverse (y : ys) result = _reverse ys (y : result)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = (f x) : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) | p x = x : filter p xs
filter p (_ : xs) = filter p xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

concat :: [[a]] -> [a]
concat [] = []
concat ([] : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)
