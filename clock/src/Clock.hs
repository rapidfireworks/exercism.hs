module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock
  { hour :: Int,
    minute :: Int
  }
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = addDelta h m Clock {hour = 0, minute = 0}

toString :: Clock -> String
toString (Clock {hour = h, minute = m}) = printf "%02d:%02d" h m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hd md (Clock {hour = h, minute = m}) =
  Clock {hour = newHour, minute = newMinute}
  where
    (carry, newMinute) = divMod (m + md) 60
    (_, newHour) = divMod (h + hd + carry) 24
