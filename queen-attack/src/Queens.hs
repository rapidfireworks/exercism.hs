module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ row <$> indices
  where
    indices = [0 .. 7]
    row x = unwords $ square <$> indices
      where
        square y
          | white == pure (x, y) = "W"
          | black == pure (x, y) = "B"
          | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) =
  x1 == x2
    || y1 == y2
    || x1 - y1 == x2 - y2
    || x1 + y1 == x2 + y2
