module Matrix
  ( saddlePoints
  ) where

import           Data.Array

getRow, getCol :: (Ix i) => i -> Array (i, i) e -> [e]
getRow n = map snd . filter (\((i, _), _) -> i == n) . assocs

getCol n = map snd . filter (\((_, i), _) -> i == n) . assocs

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = map fst $ filter f $ assocs matrix
  where
    f ((i, j), x) =
      x == maximum (getRow i matrix) && x == minimum (getCol j matrix)
