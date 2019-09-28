module DNA
  ( nucleotideCounts
  ) where

import           Data.Map (Map)
import qualified Data.Map as M

nucleotides :: String
nucleotides = "ACGT"

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
  | all (flip elem nucleotides) xs =
    Right $
    M.fromList $
    map (\n -> (n, (foldr (flip (+) . fromEnum) 0 (map (== n) xs)))) nucleotides
  | otherwise = Left "Invalid DNA sequence"
