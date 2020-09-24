module Base (Error (..), rebase) where

import Control.Monad (foldM)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
  deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits =
  rebaseFrom inputBase inputDigits
    >>= rebaseTo outputBase

rebaseFrom :: (Integral a) => a -> [a] -> Either (Error a) a
rebaseFrom inputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | otherwise = foldM go 0 inputDigits
  where
    go result digit
      | digit >= 0 && digit < inputBase = Right $ result * inputBase + digit
      | otherwise = Left $ InvalidDigit digit

rebaseTo :: Integral a => a -> a -> Either (Error a) [a]
rebaseTo outputBase n
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise = Right $ _rebaseTo outputBase n []
  where
    _rebaseTo _ 0 result = result
    _rebaseTo base k result = _rebaseTo base q (r : result)
      where
        (q, r) = quotRem k base
