module SecretHandshake (handshake) where

import Data.Bits (bit, testBit, (.&.))

handshake :: Int -> [String]
handshake n = do
  position <- positions
  let code = n .&. bit position
  decode code
  where
    positions
      | testBit n 4 = [3, 2 .. 0]
      | otherwise = [0 .. 3]

decode :: Int -> [String]
decode 1 = ["wink"]
decode 2 = ["double blink"]
decode 4 = ["close your eyes"]
decode 8 = ["jump"]
decode _ = []
