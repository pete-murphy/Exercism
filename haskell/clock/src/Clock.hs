{-# LANGUAGE ViewPatterns #-}

module Clock
  ( addDelta
  , fromHourMin
  , toString
  ) where

data Clock =
  Clock Int Int

instance Show Clock where
  show = toString

instance Eq Clock where
  Clock h m == Clock h' m' = h == h' && m == m'

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock (((h + m1 - 1) `mod` 24) + 1) m2
  where
    (m1, m2) = divMod m 60

toString :: Clock -> String
toString (Clock h m) = (pad h) ++ ":" ++ pad m
  where
    pad (show -> m')
      | length m' > 1 = m'
      | otherwise = '0' : m'

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min' (Clock h m) = fromHourMin (h + hour) (m + min')
