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
fromHourMin h m = addDelta h m (Clock 0 0)

toString :: Clock -> String
toString (Clock h m) = pad 24 h ++ ":" ++ pad 60 m
  where
    pad m' (show . flip mod m' -> n)
      | length n > 1 = n
      | otherwise = '0' : n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min' (Clock h m) = Clock (h + hour + m1) m2
  where
    (m1, m2) = divMod (m + min') 60
