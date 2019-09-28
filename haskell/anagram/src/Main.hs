{-# LANGUAGE TupleSections #-}

module Main where

import           Criterion.Main
import           Data.Char      (toLower)
import           Data.Function  (on)
import           Data.List      (sort)
import           Data.Map       (fromListWith)

isAnagramOf :: String -> String -> Bool
isAnagramOf = (==) `on` (sort . fmap toLower)

isAnagramOf' :: String -> String -> Bool
isAnagramOf' = (==) `on` freqs
  where
    freqs = fromListWith (+) . fmap ((, 1) . toLower)

main :: IO ()
main = do
  everyWord <- lines <$> readFile "/usr/share/dict/words"
  defaultMain
    [ bench "anagrams, sort" $ whnf (isAnagramOf <$> everyWord <*>) everyWord
    , bench "anagrams, freqs" $ whnf (isAnagramOf' <$> everyWord <*>) everyWord
    ]
