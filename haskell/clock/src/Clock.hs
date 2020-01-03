{-# LANGUAGE ViewPatterns #-}

module Clock
  ( addDelta
  , fromHourMin
  , toString
  ) where

import           Data.Function (on)

data Clock =
  Clock Int Int

instance Show Clock where
  show = toString

instance Eq Clock where
  (==) = (==) `on` show

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock (h + m1) m2
  where
    (m1, m2) = divMod m 60

toString :: Clock -> String
toString (Clock h m) = padHours h ++ ":" ++ padMinutes m
  where
    padHours (show . flip mod 24 -> h')
      | h' == "00" = "24"
      | length h' > 1 = h'
      | otherwise = '0' : h'
    padMinutes (show . flip mod 60 -> m')
      | length m' > 1 = m'
      | otherwise = '0' : m'

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min' (Clock h m) = Clock (h + hour + m1) m2
  where
    (m1, m2) = divMod (m + min') 60
