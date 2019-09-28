module Raindrops
  ( convert
  ) where

import           Data.Maybe (catMaybes)

convert :: Int -> String
convert n =
  case all (== Nothing) rainSoundsFromFactors of
    True -> show n
    _    -> concat $ catMaybes rainSoundsFromFactors
  where
    rainSounds = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    rainSoundsFromFactors = (map (`lookup` rainSounds) (factors n))

factors :: Int -> [Int]
factors n = filter ((== 0) . mod n) [1 .. n]
