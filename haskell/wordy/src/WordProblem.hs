{-# LANGUAGE BlockArguments #-}

module WordProblem (answer) where

import Data.Functor.Alt (Alt(..))
import Prelude hiding (lex)
import Data.Char
-- import Text.Read hiding (lex)

data Arith
  = Add Arith Arith
  | Sub Arith Arith
  | Mul Arith Arith
  | Div Arith Arith
  | Pow Arith Arith
  | Lit Integer

eval :: Arith -> Integer
eval (Lit n) = n
eval (Add n m) = eval n + eval m
eval (Sub n m) = eval n - eval m
eval (Mul n m) = eval n * eval m
eval (Div n m) = eval n `div` eval m
eval (Pow n m) = eval n ^ eval m

newtype Token = Token String

newtype Parser a = Parser { runParser :: [Token] -> (Maybe a, [Token]) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser \tokens ->
      let (a, b) = p tokens
       in (f <$> a, b)

instance Applicative Parser where
  pure x = Parser \tokens -> (Just x, tokens)
  Parser pf <*> Parser p =
    Parser \tokens ->
      let (f, ts) = pf tokens
          (x, ts') = p ts
       in (f <*> x, ts')

instance Alt Parser where
  Parser f <!> Parser g =
    Parser \tokens ->
      let results = f tokens
       in case fst results of
            Nothing -> g tokens
            _       -> results


lex :: String -> [Token]
lex = fmap Token . words . filter isAlphaNum

parseArith :: Parser Arith
parseArith = undefined

-- parseAdd 

answer :: String -> Maybe Integer
answer = fmap eval . fst . runParser parseArith . lex
  