module Change
  ( findFewestCoins
  ) where

import           Data.List  (tails)
import           Data.Maybe (listToMaybe)

draw :: Integral n => [a] -> n -> [[a]]
draw _ 0  = [[]]
draw xs n = [t : rest | ts@(t:_) <- tails xs, rest <- draw ts (n - 1)]

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins@(c:_)
  | target `mod` foldl1 gcd coins /= 0 = Nothing
  | otherwise = listToMaybe $ filter ((== target) . sum) paths
  where
    paths = concatMap (draw coins) [0 .. div target c]
