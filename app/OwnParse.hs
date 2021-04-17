{-# LANGUAGE DeriveLift #-}

module OwnParse where

import Control.Monad       (liftM, ap)
import Data.Char           (ord)

data Parser a = P (String -> [(a, String)])

parse :: Parser a -> (String -> [(a, String)])
parse (P f) = f

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  (<*>) = ap
  pure v = P (\inp -> [(v, inp)])

instance Monad Parser where
  (P p) >>= f = P (\inp -> concat [parse (f v) $ inp' | (v, inp') <- p inp])

class Monad m => Monad0Plus m where
  zero :: m a
  (+++) :: m a -> m a -> m a

instance Monad0Plus Parser where
  zero = P (\inp -> [])
  p +++ q = first (P (\inp -> ((parse p) inp ++ (parse q) inp)))

first :: Parser a -> Parser a
first p = P (\inp -> case (parse p) inp of
  [] -> []
  (x:xs) -> [x])

item :: Parser Char
item = P (\inp -> case inp of
  []     -> []
  (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

isPunc :: Char -> Bool
isPunc x = (x == '?') || (x == '.') || (x == '!')

punc :: Parser Char
punc = sat isPunc

sent :: Parser Char
sent = letter +++ punc +++ (sat isSpace)

letter :: Parser Char
letter = lower +++ upper

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- force :: Parser a -> Parser a
-- force p = P (\inp -> let x = (parse p) inp in
--   (fst (head x), snd (head x)) : tail x)

many :: Parser a -> Parser [a]
many p = (do
  x <- p
  xs <- many p
  return (x:xs)) +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  open
  x <- p
  close
  return x

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sepHelp p sep)
  return (x:xs)

sepHelp :: Parser a -> Parser b -> Parser a
sepHelp p sep = do
  sep
  y <- p
  return y

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (sepBy1 p sep) +++ return []

isSpace :: Char -> Bool
isSpace x = (x == ' ') || (x == '\n') || (x == '\t')

spaces :: Parser ()
spaces = do
  many1 (sat isSpace)
  return ()

junk :: Parser ()
junk = do
  many spaces
  return ()

front :: Parser a -> Parser a
front p = do
  junk
  v <- p
  return v

token :: Parser a -> Parser a
token p = do
  v <- p
  junk
  return v

word :: Parser String
word = do
  x <- letter
  xs <- word
  return (x:xs) +++ return ""
