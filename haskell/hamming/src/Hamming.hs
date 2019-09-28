module Hamming
  ( distance
  ) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ sum differences
  where
    differences = zipWith diff xs ys
    diff x y
      | x == y = 0
      | otherwise = 1
