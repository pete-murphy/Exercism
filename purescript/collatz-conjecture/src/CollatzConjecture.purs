module CollatzConjecture
  ( collatz
  ) where

import Prelude
import Control.Monad.ST.Internal (ST, STRef)
import Control.Monad.ST.Internal as ST
import Data.Int (even)
import Data.Maybe (Maybe(..))

collatz :: Int -> Maybe Int
collatz n
  | n == 1 = Just 0
  | n < 1 = Nothing
  | otherwise = Just (countSteps n)

countSteps :: Int -> Int
countSteps n =
  ST.run do
    m <- ST.new n
    stepCount <- ST.new 0
    ST.while ((_ /= 1) <$> ST.read m) do
      runStep m *> increment stepCount
    ST.read stepCount

runStep :: forall r. STRef r Int -> ST r Int
runStep m = ST.modify (\m' -> if even m' then m' `div` 2 else 3 * m' + 1) m

increment :: forall r. STRef r Int -> ST r Int
increment = ST.modify (_ + 1)
