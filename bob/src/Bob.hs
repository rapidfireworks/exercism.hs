module Bob (responseFor) where

import Text.Regex.TDFA ((=~))

responseFor :: String -> String
responseFor xs
  | silent = "Fine. Be that way!"
  | yelling && asking = "Calm down, I know what I'm doing!"
  | asking = "Sure."
  | yelling = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    silent = xs =~ "\\`[[:space:]]*\\'"
    yelling = not (xs =~ "[[:lower:]]") && xs =~ "[[:upper:]]"
    asking = xs =~ "\\?[[:space:]]*\\'"
