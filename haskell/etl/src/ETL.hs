module ETL
  ( transform
  ) where

import           Data.Char (toLower)
import           Data.Map  (Map, fromList, toList)

transform :: Map a String -> Map Char a
transform =
  fromList .
  foldr (\(k, v) -> (++) (map (\v' -> (toLower v', k)) v)) [] . toList
