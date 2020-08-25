module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz = collatz 0
  where
    collatz _ x
      | x < 1 = Nothing
    collatz n 1 = Just n
    collatz n x
      | r == 0 = collatz (n + 1) q
      | otherwise = collatz (n + 1) (3 * x + 1)
      where
        (q, r) = quotRem x 2
