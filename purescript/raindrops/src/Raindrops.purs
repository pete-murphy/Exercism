module Raindrops (raindrops) where

import Prelude
import Data.Array (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

isDivisibleBy :: Int -> Int -> Boolean
isDivisibleBy n m = n `mod` m == 0

raindrops :: Int -> String
raindrops n =
  fromMaybe (show n)
    ( fold
        ( [ Tuple 3 "Pling"
          , Tuple 5 "Plang"
          , Tuple 7 "Plong"
          ]
            <#> \(Tuple m p) ->
                if n `isDivisibleBy` m then Just p else Nothing
        )
    )
