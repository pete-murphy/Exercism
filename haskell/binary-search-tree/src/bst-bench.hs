{-# LANGUAGE BlockArguments #-}

module Main where

import BST (BST)
import qualified BST
import Criterion.Main

bst :: BST Int
bst = BST.fromList [1 .. 9999]

main :: IO ()
main =
  defaultMain
    [ bench "toList using foldr" do
        whnf BST.toList bst,
      bench "toList using foldMap" do
        whnf BST.toList' bst,
      bench "toList using (++)" do
        whnf BST.toList'' bst
    ]
