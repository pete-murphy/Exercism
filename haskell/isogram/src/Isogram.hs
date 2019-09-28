module Isogram
  ( isIsogram
  ) where

import           Data.Char

isIsogram :: String -> Bool
isIsogram s
  | s == [] = True
  | otherwise = go s []
  where
    go w acc
      | cs == "" = not (elem c cs || elem c acc)
      | elem c cs || elem c acc = False
      | otherwise = go cs (c : acc)
      where
        (c:cs) = map toLower $ filter isLetter w
