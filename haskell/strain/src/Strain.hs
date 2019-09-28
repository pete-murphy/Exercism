module Strain
  ( keep
  , discard
  ) where

-- discard = filter . (not .)
discard :: (a -> Bool) -> [a] -> [a]
discard p (xs) =
  foldr
    ((++) .
     (\x ->
        if p x
          then []
          else [x]))
    []
    xs

-- keep = filter
keep :: (a -> Bool) -> [a] -> [a]
keep p (xs) =
  foldr
    ((++) .
     (\x ->
        if p x
          then [x]
          else []))
    []
    xs
