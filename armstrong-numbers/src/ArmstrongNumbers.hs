module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = armstrongSum == toInteger n
  where
    armstrongSum = sum $ do
      digit <- show $ toInteger n
      return (read [digit] ^ digitCount n)

digitCount :: (Integral a) => a -> a
digitCount n | n < 0 = digitCount $ abs n
digitCount 0 = 1
digitCount n = 1 + truncLog n
  where
    truncLog = truncate . log10 . fromIntegral

log10 :: Double -> Double
log10 = logBase 10
