{-# LANGUAGE LambdaCase #-}

module TwelveDays
  ( recite
  ) where

recite :: Int -> Int -> [String]
recite start stop = map reciteLine [start .. stop]

reciteLine :: Int -> String
reciteLine n =
  "On the " ++
  nthDay ++ " day of Christmas my true love gave to me: " ++ go n ++ "."
  where
    (nthDay, _) = nth n
    thing = snd . nth
    go m
      | n == 1 = thing n
      | m == 1 = "and " ++ (thing m)
      | otherwise = thing m ++ ", " ++ go (m - 1)

nth :: Int -> (String, String)
nth =
  \case
    1 -> ("first", "a Partridge in a Pear Tree")
    2 -> ("second", "two Turtle Doves")
    3 -> ("third", "three French Hens")
    4 -> ("fourth", "four Calling Birds")
    5 -> ("fifth", "five Gold Rings")
    6 -> ("sixth", "six Geese-a-Laying")
    7 -> ("seventh", "seven Swans-a-Swimming")
    8 -> ("eighth", "eight Maids-a-Milking")
    9 -> ("ninth", "nine Ladies Dancing")
    10 -> ("tenth", "ten Lords-a-Leaping")
    11 -> ("eleventh", "eleven Pipers Piping")
    _ -> ("twelfth", "twelve Drummers Drumming")
