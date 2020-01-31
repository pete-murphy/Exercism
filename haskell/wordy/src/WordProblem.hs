{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module WordProblem (answer) where

import Data.Functor.Alt (Alt(..), many)
import Data.List (foldl')
import Prelude hiding (lex)
import Text.Read (readMaybe)

data Arith
  = Add Arith Arith
  | Sub Arith Arith
  | Mul Arith Arith
  | Div Arith Arith
  | Pow Arith Arith
  | Lit Integer
  deriving Show

eval :: Arith -> Integer
eval (Lit n) = n
eval (Add n m) = eval n + eval m
eval (Sub n m) = eval n - eval m
eval (Mul n m) = eval n * eval m
eval (Div n m) = eval n `div` eval m
eval (Pow n m) = eval n ^ eval m

newtype Token = Token { unToken :: String } deriving Show

newtype Parser a = Parser { runParser :: [Token] -> Maybe ([Token], a) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser \tokens ->
      fmap (fmap f) (p tokens)

instance Applicative Parser where
  pure x = Parser \tokens -> Just (tokens, x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser p =
    Parser \tokens -> do
      (ts, f) <- pf tokens
      (ts', a) <- p ts
      pure (ts', f a)

instance Alt Parser where
  Parser f <!> Parser g =
    Parser \tokens ->
      let results = f tokens
       in case results of
          Nothing -> g tokens
          _       -> results

lex :: String -> [Token]
lex = fmap (Token . filter (/= '?')) . words

parseArith :: Parser Arith
parseArith = do
  _ <- parseQuestion
  x <- parseLit
  fs <- many (parseAdd <!> parseMul <!> parseDiv <!> parseSub)
  _ <- parseEnd
  pure (foldl' (flip ($)) x fs)

parseQuestion :: Parser ()
parseQuestion = Parser \case
  (Token "What":Token "is":xs) -> pure (xs, ())
  _ -> Nothing

parseEnd :: Parser ()
parseEnd = Parser \case
  [] -> pure ([], ())
  _ -> Nothing

parseAdd :: Parser (Arith -> Arith)
parseAdd = Parser \case 
  (Token "plus":Token x:xs) -> do
    x' <- readMaybe x
    pure (xs, Add (Lit x'))
  _ -> Nothing

parseMul :: Parser (Arith -> Arith)
parseMul = Parser \case 
  (Token "multiplied":Token "by":Token x:xs) -> do
    x' <- readMaybe x
    pure (xs, Mul (Lit x'))
  _ -> Nothing

parseDiv :: Parser (Arith -> Arith)
parseDiv = Parser \case 
  (Token "divided":Token "by":Token x:xs) -> do
    x' <- readMaybe x
    pure (xs, flip Div (Lit x'))
  _ -> Nothing

parseSub :: Parser (Arith -> Arith)
parseSub = Parser \case 
  (Token "minus":Token x:xs) -> do
    x' <- readMaybe x
    pure (xs, flip Sub (Lit x'))
  _ -> Nothing

parseLit :: Parser Arith
parseLit = Parser \case
  (Token x:xs) -> do
    x' <- readMaybe x
    pure (xs, Lit x')
  _ -> Nothing

answer :: String -> Maybe Integer
answer = fmap (eval . snd) . runParser parseArith . lex