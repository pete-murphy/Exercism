module DNA
  ( toRNA
  ) where

toRNA :: String -> Maybe String
toRNA = traverse (`lookup` pairs)
  where
    pairs = [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')]
