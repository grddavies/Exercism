module RunLength (decode, encode) where

import Data.List (groupBy)
import Data.Maybe (fromJust)
import Control.Applicative (Alternative (..))
import Data.Char (isDigit)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> fmap f <$> (p input)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  Parser ab <*> Parser a = Parser $ \input -> do
    (r1, f) <- ab input
    (r2, x) <- a r1
    return (r2, f x)

instance Monad Parser where
  Parser a >>= f = Parser $ \input -> do
      (rest, x) <- a input
      let Parser b = f x
      (rest', y) <- b rest
      return (rest', y)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Nothing -> b input
      Just x -> Just x
    
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input ->
  case input of
    [] -> Nothing
    x:xs -> if pred x then Just (xs, x) else Nothing

charP :: Char -> Parser Char
charP c = satisfy (==c)

anyCharP :: Parser Char
anyCharP = satisfy (not.isDigit)

stringP :: String -> Parser String
stringP = traverse charP

digitP :: Parser Char
digitP = satisfy isDigit

intP :: Parser Int
intP = read <$> some digitP

runLengthParser :: Parser String
runLengthParser = do
  n <- intP
  c <- anyCharP
  return $ replicate n c

parse :: Parser a -> String -> a
parse p input = case runParser p $ input of
  Nothing -> error "no parse"
  Just (_, x) -> x

decode :: String -> String
decode =  parse $ concat <$> many (runLengthParser <|> pure <$> anyCharP)

encode :: String -> String
encode = concatMap runLength . (groupBy (==))
  where
   runLength :: String -> String
   runLength run@(h:_)
      | length run == 1 = [h]
      | otherwise = show (length run) <> [h]

