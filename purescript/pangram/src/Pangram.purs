module Pangram
  ( isPangram
  ) where

import Prelude
import Data.Enum (enumFromTo)
import Data.Set (Set, fromFoldable, subset)
import Data.String (toLower)
import Data.String.CodeUnits (toCharArray)

allCharsSet :: Set Char
allCharsSet = fromFoldable (enumFromTo 'a' 'z' :: Array Char)

isPangram :: String -> Boolean
isPangram = toLower >>> toCharArray >>> fromFoldable >>> subset allCharsSet
