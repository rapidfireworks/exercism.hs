module Matrix
  ( Matrix,
    cols,
    column,
    flatten,
    fromList,
    fromString,
    reshape,
    row,
    rows,
    shape,
    transpose,
  )
where

import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Text.Read (readMaybe)

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix xss) = V.foldl' max 0 $ V.length <$> xss

column :: Int -> Matrix a -> Vector a
column x (Matrix xss) = xss <&> (! pred x)

flatten :: Matrix a -> Vector a
flatten (Matrix xss) = V.concatMap id xss

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList $ V.fromList <$> xss

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ do
  line <- T.lines $ T.pack xs
  return $ do
    word <- T.words line
    Just n <- pure $ readMaybe $ T.unpack word
    return n

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix = Matrix $ do
  startIndex <- V.enumFromStepN 0 c r
  return $ V.slice startIndex c xs
  where
    xs = flatten matrix

row :: Int -> Matrix a -> Vector a
row x (Matrix xss) = xss ! pred x

rows :: Matrix a -> Int
rows (Matrix xss) = V.length xss

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ do
  x <- V.enumFromN 1 $ cols matrix
  return $ column x matrix
