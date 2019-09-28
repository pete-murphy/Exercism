{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module Anagram
  ( anagramsFor
  ) where

import           Data.Char     (toLower)
import           Data.Function (on)

import           Data.Map      (Map)
import qualified Data.Map      as M

anagramsFor :: String -> [String] -> [String]
anagramsFor (map toLower -> cs) =
  filter (((&&) <$> (/= cs) <*> ((==) `on` occurrences) cs) . fmap toLower)
  where
    occurrences :: String -> Map Char Int
    occurrences = M.fromListWith (+) . fmap (, 1)
