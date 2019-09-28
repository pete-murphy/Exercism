module Brackets
  ( arePaired
  ) where

brackets :: [(Char, Char)]
brackets = [('[', ']'), ('{', '}'), ('(', ')')]

isBracket :: Char -> Bool
isBracket = flip elem (members' brackets)
  where
    members' = (++) <$> (map fst) <*> (map snd)

closes :: Char -> Char -> Bool
closes c d = (d, c) `elem` brackets

isOpen :: Char -> Bool
isOpen = flip elem $ map fst brackets

parse :: String -> String -> Bool
parse acc [] = acc == []
parse [] (x:xs)
  | isOpen x = parse [x] xs
  | otherwise = False
parse acc@(a:as) (x:xs)
  | isOpen x = parse (x : acc) xs
  | x `closes` a = parse as xs
  | otherwise = False

arePaired :: String -> Bool
arePaired xs = parse [] (filter isBracket xs)
