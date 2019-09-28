module SumOfMultiples
  ( sumOfMultiples
  ) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $
  filter
    (\x -> any (== 0) (map (\y -> (x `mod` y)) factors))
    [1 .. (limit - 1)]
