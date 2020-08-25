module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz = collatz 0
  where
    collatz n x
      | x < 1 = Nothing
      | x == 1 = Just n
      | r == 0 = collatz (n + 1) q
      | otherwise = collatz (n + 1) (3 * x + 1)
      where
        (q, r) = quotRem x 2
