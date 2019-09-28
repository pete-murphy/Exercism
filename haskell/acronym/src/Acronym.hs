module Acronym
  ( abbreviate
  ) where

import           Data.Char (isLower, isPunctuation, isUpper, toUpper)

abbreviate :: String -> String
abbreviate =
  map toUpper .
  concat .
  map
    (\w ->
       if (all isUpper w || all isLower w)
         then ([(head w)])
         else (filter isUpper w)) .
  words .
  map
    (\c ->
       if (isPunctuation c)
         then ' '
         else c)
