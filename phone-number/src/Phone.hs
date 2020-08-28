module Phone (number) where

import Data.Char (isNumber)
import Data.Function ((&))

number :: String -> Maybe String
number xs =
  filter isNumber xs
    & stripCountryCode
    & validateNanp

stripCountryCode :: String -> String
stripCountryCode ('1' : ns) = ns
stripCountryCode ns = ns

validateNanp :: String -> Maybe String
validateNanp ns@([n1, _, _, n4, _, _, _, _, _, _])
  | all isGreaterThanOne [n1, n4] = Just ns
  where
    isGreaterThanOne x = elem x ['2' .. '9']
validateNanp _ = Nothing
