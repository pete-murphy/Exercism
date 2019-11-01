module Accumulate
  ( accumulate
  ) where

import Prelude
import Data.List (List(..), foldr, (:))

accumulate :: forall a b. (a -> b) -> List a -> List b
accumulate f = foldr (f >>> (:)) Nil

accumulate' :: forall a b. (a -> b) -> List a -> List b
accumulate' _ Nil = Nil

accumulate' f (x : xs) = f x : accumulate f xs
