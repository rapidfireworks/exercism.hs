module Meetup (Weekday (..), Schedule (..), meetupDay) where

import Data.Time.Calendar (Day)
import Data.Time.Calendar as Calendar (addDays, dayOfWeek, fromGregorian)

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Enum)

data Schedule = First | Second | Third | Fourth | Last | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = addDays offset base
  where
    baseDay = case schedule of
      First -> 1
      Second -> 8
      Teenth -> 13
      Third -> 15
      Fourth -> 22
      Last -> 31
    base = fromGregorian year month baseDay
    offset = case (schedule, weekdayOffset weekday base) of
      (Last, 0) -> 0
      (Last, dx) -> dx - 7
      (_, dx) -> dx

weekdayOffset :: Weekday -> Day -> Integer
weekdayOffset weekday day = fromIntegral $ mod diff 7
  where
    start = fromEnum $ dayOfWeek day
    end = fromEnum weekday
    diff = end - start
