module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n | n < 0 = False
armstrong n = armstrongSum == toInteger n
  where
    digits = show $ toInteger n
    digitCount = length digits
    armstrongSum = sum [read [digit] ^ digitCount | digit <- digits]
