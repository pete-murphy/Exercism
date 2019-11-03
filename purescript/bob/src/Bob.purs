module Bob
  ( hey
  ) where

import Prelude
import Data.Array (all, filter, last)
import Data.Char.Unicode (isAlpha, isUpper)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Data.String.CodeUnits (toCharArray)

hey :: String -> String
hey prompt = case question /\ yelling /\ notSayingAnything of
  true /\ false /\ _ -> "Sure."
  _ /\ true /\ _ -> "Whoa, chill out!"
  _ /\ _ /\ true -> "Fine. Be that way!"
  otherwise -> "Whatever."
  where
  prompt' = toCharArray $ trim prompt

  question = last prompt' == Just '?'

  yelling = (&&) <$> all isUpper <*> not (_ == []) $ filter isAlpha prompt'

  notSayingAnything = prompt' == []
