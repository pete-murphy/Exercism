module TwelveDays
  ( recite,
  )
where

recite :: Int -> Int -> [String]
recite start end
  | start > end = []
  | otherwise =
    let nth = nths !! (start - 1)
        gs = reverse (take start gifts)
     in reciteLine nth gs : recite (start + 1) end

newtype Nth = Nth String

newtype Gift = Gift String

reciteLine :: Nth -> [Gift] -> String
reciteLine (Nth nth) gifts = "On the " <> nth <> " day of Christmas my true love gave to me: " <> joinGifts gifts <> "."

joinGifts :: [Gift] -> String
joinGifts [Gift g] = g
joinGifts gs = go gs
  where
    go [Gift g] = "and " <> g
    go (Gift g : gs') = g <> ", " <> go gs'

gifts :: [Gift]
gifts =
  map
    Gift
    [ "a Partridge in a Pear Tree",
      "two Turtle Doves",
      "three French Hens",
      "four Calling Birds",
      "five Gold Rings",
      "six Geese-a-Laying",
      "seven Swans-a-Swimming",
      "eight Maids-a-Milking",
      "nine Ladies Dancing",
      "ten Lords-a-Leaping",
      "eleven Pipers Piping",
      "twelve Drummers Drumming"
    ]

nths :: [Nth]
nths =
  map
    Nth
    [ "first",
      "second",
      "third",
      "fourth",
      "fifth",
      "sixth",
      "seventh",
      "eighth",
      "ninth",
      "tenth",
      "eleventh",
      "twelfth"
    ]
