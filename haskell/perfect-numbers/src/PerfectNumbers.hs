module PerfectNumbers
  ( classify
  , Classification(..)
  ) where

data Classification
  = Deficient
  | Perfect
  | Abundant
  deriving (Eq, Show)

factors :: Int -> [Int]
factors n = go [] (n - 1)
  where
    go acc n'
      | n' == 0 = acc
      | n' > 0 = go (isFactor n n' ++ acc) (n' - 1)
    isFactor m m'
      | mod m m' == 0 = [m']
      | otherwise = []

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise = Just c
  where
    c
      | sum (factors n) > n = Abundant
      | sum (factors n) < n = Deficient
      | sum (factors n) == n = Perfect
