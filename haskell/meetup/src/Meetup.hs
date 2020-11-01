{-# LANGUAGE TypeApplications #-}

module Meetup
  ( Weekday (..),
    Schedule (..),
    meetupDay,
  )
where

import Data.Time.Calendar (Day, DayOfWeek, addDays, dayOfWeek, fromGregorian)

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Read, Show)

instance Enum Weekday where
  toEnum = read . show . toEnum @DayOfWeek
  fromEnum = fromEnum . read @DayOfWeek . show

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = error "You need to implement this function."

nextWeekday :: Weekday -> Day -> Day
nextWeekday w d
  | w' == w = d
  | w' > w = addDays (fromIntegral $ length [w .. w']) d
  | otherwise = addDays (fromIntegral $ length [w' .. w]) d
  where
    w' = weekDay d

weekdayOfFirstOfMonth :: Integer -> Int -> Weekday
weekdayOfFirstOfMonth year month = weekDay (fromGregorian year month 1)

weekDay :: Day -> Weekday
weekDay = toEnum . fromEnum . dayOfWeek
