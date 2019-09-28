module Pangram
  ( isPangram
  ) where

import           Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (flip elem text') ['a' .. 'z']
  where
    text' = map toLower text
