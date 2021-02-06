module Raindrops (raindrops) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

raindrops :: Int -> String
raindrops n =
  fromMaybe (show n) do
    Array.foldMap
      (\(Tuple m p) -> if n `mod` m == 0 then Just p else Nothing)
      [ Tuple 3 "Pling"
      , Tuple 5 "Plang"
      , Tuple 7 "Plong"
      ]
