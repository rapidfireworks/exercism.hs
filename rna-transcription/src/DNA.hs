module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = mapM complement xs
  where
    complement 'G' = Right 'C'
    complement 'C' = Right 'G'
    complement 'T' = Right 'A'
    complement 'A' = Right 'U'
    complement x = Left x
