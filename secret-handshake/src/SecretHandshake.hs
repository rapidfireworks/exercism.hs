module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = do
  position <- positions
  decode (n .&. (bit position))
  where
    positions
      | n .&. bit 4 /= 0 = [3, 2 .. 0]
      | otherwise = [0 .. 3]

decode :: Int -> [String]
decode 1 = ["wink"]
decode 2 = ["double blink"]
decode 4 = ["close your eyes"]
decode 8 = ["jump"]
decode _ = []
