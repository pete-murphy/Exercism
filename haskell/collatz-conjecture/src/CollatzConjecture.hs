module CollatzConjecture
  ( collatz
  ) where

collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just $ collatz' (n, 0)
  | otherwise = Nothing
  where
    collatz' (n', s)
      | n' == 1 = s
      | even n' = collatz' (div n' 2, s + 1)
      | otherwise = collatz' (3 * n' + 1, s + 1)
