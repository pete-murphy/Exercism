module Bob
  ( responseFor
  ) where

import           Data.Char (isLower, isSpace, isUpper)

responseFor :: String -> String
responseFor xs
  | isEmpty = "Fine. Be that way!"
  | isShout && not isQuestion = "Whoa, chill out!"
  | isShout = "Calm down, I know what I'm doing!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    isShout = (not (any isLower xs)) && any isUpper xs
    isQuestion = (== '?') $ last $ last $ words xs
    isEmpty = all isSpace xs
