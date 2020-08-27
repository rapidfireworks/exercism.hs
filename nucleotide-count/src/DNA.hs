module DNA (nucleotideCounts, Nucleotide (..)) where

import Control.Monad (foldM)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map (empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM countM Map.empty xs
  where
    countM acc val = fromChar val <&> count acc
    count acc val = Map.insertWith (+) val 1 acc
    fromChar 'A' = Right A
    fromChar 'C' = Right C
    fromChar 'G' = Right G
    fromChar 'T' = Right T
    fromChar _ = Left xs
