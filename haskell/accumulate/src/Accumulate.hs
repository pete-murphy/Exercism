module Accumulate
  ( accumulate
  ) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate = map

accumulate' :: (a -> b) -> [a] -> [b]
accumulate' _ []     = []
accumulate' f (x:xs) = (f x) : accumulate' f xs

accumulate'' :: (a -> b) -> [a] -> [b]
accumulate'' f xs = foldr ((:) . f) [] xs
