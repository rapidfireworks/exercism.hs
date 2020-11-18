module OCR (convert) where

import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)

convert :: String -> String
convert = intercalate "," . map parseLine . chunksOf 4 . lines
  where
    parseLine = map parse . transpose . map (chunksOf 3)

parse :: [String] -> Char
parse [" _ ", "| |", "|_|", "   "] = '0'
parse ["   ", "  |", "  |", "   "] = '1'
parse [" _ ", " _|", "|_ ", "   "] = '2'
parse [" _ ", " _|", " _|", "   "] = '3'
parse ["   ", "|_|", "  |", "   "] = '4'
parse [" _ ", "|_ ", " _|", "   "] = '5'
parse [" _ ", "|_ ", "|_|", "   "] = '6'
parse [" _ ", "  |", "  |", "   "] = '7'
parse [" _ ", "|_|", "|_|", "   "] = '8'
parse [" _ ", "|_|", " _|", "   "] = '9'
parse _ = '?'
