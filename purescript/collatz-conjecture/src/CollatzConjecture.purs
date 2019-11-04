module CollatzConjecture
  ( collatz
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.ST.Internal as ST
import Data.Int (even)

collatz :: Int -> Maybe Int
collatz n
  | n == 1 = Just 0
  | n < 1 = Nothing
  | otherwise =
    Just
      $ ST.run do
          m <- ST.new n
          acc <- ST.new 0
          ST.while ((_ /= 1) <$> ST.read m)
            $ ST.modify (\m' -> if even m' then div m' 2 else 3 * m' + 1) m
            *> ST.modify (_ + 1) acc
          ST.read acc
