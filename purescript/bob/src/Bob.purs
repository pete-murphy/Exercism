module Bob
  ( hey
  ) where

import Prelude
import Data.Array (all, filter, last)
import Data.Char.Unicode (isAlpha, isUpper)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.String.CodeUnits (toCharArray)

hey :: String -> String
hey prompt =
  let
    prompt' = toCharArray $ trim prompt

    question = last prompt' == Just '?'

    yelling = (&&) <$> all isUpper <*> not (_ == []) $ filter isAlpha prompt'

    notSayingAnything = prompt' == []
  in
    case unit of
      _
        | question && not yelling -> "Sure."
      _
        | yelling -> "Whoa, chill out!"
      _
        | notSayingAnything -> "Fine. Be that way!"
      _
        | otherwise -> "Whatever."
