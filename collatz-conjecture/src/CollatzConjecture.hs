module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
  | x > 0 = Just (count 0 x)
  | otherwise = Nothing
  where
    count n 1 = n
    count n y
      | r == 0 = count (n + 1) q
      | otherwise = count (n + 1) (3 * y + 1)
      where
        (q, r) = quotRem y 2
