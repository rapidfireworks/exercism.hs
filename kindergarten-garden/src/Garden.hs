module Garden (Plant (..), garden, lookupPlants) where

import Data.Map (Map)
import Data.Map as Map (findWithDefault, fromList)

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

garden :: [String] -> String -> Map String [Plant]
garden students = Map.fromList . zip students . divide . lines

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants student = Map.findWithDefault [] student

divide :: [String] -> [[Plant]]
divide rows | all ([] ==) rows = []
divide rows = division : divide rest
  where
    division = do
      row <- rows
      letter <- take 2 row
      toPlant letter
    rest = map (drop 2) rows

toPlant :: Char -> [Plant]
toPlant 'C' = [Clover]
toPlant 'G' = [Grass]
toPlant 'R' = [Radishes]
toPlant 'V' = [Violets]
toPlant _ = []
