module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

decode :: Text -> Text
decode = T.map convert . sanitize

encode :: Text -> Text
encode = space . T.map convert . sanitize

sanitize :: Text -> Text
sanitize = T.filter isAlphaNum . T.toLower

convert :: Char -> Char
convert x
  | isAlpha x = toEnum result
  | otherwise = x
  where
    result = fromEnum 'z' - fromEnum x + fromEnum 'a'

space :: Text -> Text
space = T.unwords . T.chunksOf 5
