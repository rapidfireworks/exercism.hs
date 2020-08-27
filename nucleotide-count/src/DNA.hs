module DNA (nucleotideCounts, Nucleotide (..)) where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM count Map.empty xs
  where
    count acc val = case (fromChar val) of
      Just nucleotide -> Right (Map.insertWith (+) nucleotide 1 acc)
      Nothing -> Left xs
    fromChar 'A' = Just A
    fromChar 'C' = Just C
    fromChar 'G' = Just G
    fromChar 'T' = Just T
    fromChar _ = Nothing
