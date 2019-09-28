module Diamond
  ( diamond
  ) where

import           Data.Char (isUpper)

diamond :: Char -> Maybe [String]
diamond c
  | isUpper c = Just $ mirror $ map mirror $ withSpaces c
  | otherwise = Nothing
  where
    withSpaces k =
      map (\c -> (spaceBefore c k) ++ c : (spaceAfter c k)) ['A' .. k]
    spaceBefore x y = replicate (length [x .. y] - 1) ' '
    spaceAfter x y = replicate (length ['A' .. x] - 1) ' '
    mirror xs = xs ++ (drop 1 $ reverse xs)
