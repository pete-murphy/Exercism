module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode str = toStringDecode (toPairsDecode str)

toPairsDecode :: String -> [(Int, Char)]
toPairsDecode "" = []
toPairsDecode str =
  let (n, c : remainder) = span isDigit str
   in case n of
        "" -> (1, c) : toPairsDecode remainder
        n' -> (read n', c) : toPairsDecode remainder

toStringDecode :: [(Int, Char)] -> String
toStringDecode = concatMap (\(n, c) -> replicate n c)

---

encode :: String -> String
encode text = pairsToString (sumPairs (toPairs text))

toPairs :: String -> [(Int, Char)]
toPairs str = map (\c -> (1, c)) str

sumPairs :: [(Int, Char)] -> [(Int, Char)]
sumPairs pairs = foldr go [] pairs
  where
    go :: (Int, Char) -> [(Int, Char)] -> [(Int, Char)]
    go x [] = [x]
    go (n, c) ((n', c') : xs)
      | c == c' = (n + n', c) : xs
      | otherwise = (n, c) : (n', c') : xs

-- >>> pairsToString' []
-- ""
pairsToString' :: [(Int, Char)] -> String
pairsToString' [] = ""
pairsToString' ((1, c) : xs) = c : pairsToString xs
pairsToString' ((n, c) : xs) = show n <> (c : pairsToString xs)

pairsToString :: [(Int, Char)] -> String
pairsToString = concatMap (\(n, c) -> if n == 1 then [c] else show n <> [c])
