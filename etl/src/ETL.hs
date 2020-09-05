module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, toList)

transform :: Map a String -> Map Char a
transform = Map.fromList . migrate
  where
    migrate legacy = do
      (point, text) <- Map.toList legacy
      letter <- text
      return (toLower letter, point)
