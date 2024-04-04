{-# LANGUAGE OverloadedStrings #-}

module Parsers (
    Parser , runParser
  , emptyString
  , charPredicate
  , charPredicate_
  , wordPredicate
  , wordPredicate1
  , char_
  , word_
  , whitespace1_
  , whitespace_
  , int
  , float
  , escapeString
) where

import           Control.Applicative
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as I
import           Data.Char
import           Data.Word

------------------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: B.ByteString -> Maybe (B.ByteString, a) }

instance Functor Parser where
  fmap f (Parser g) = Parser $ (fmap . fmap . fmap $ f) g

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  f <*> g = Parser $ \input -> do
    (rest', parsedFunc) <- runParser f input
    (rest, parsedVal)   <- runParser g rest'
    return (rest, parsedFunc parsedVal)

instance Alternative Parser where
  empty = Parser $ const Nothing
  f <|> g = Parser $ \input -> runParser f input <|> runParser g input

instance Monad Parser where
  pa >>= f = Parser $ \input -> do
    (rest', parsedA) <- runParser pa input
    runParser (f parsedA) rest'

------------------------------------------------------------------------------------------

emptyString :: Parser ()
emptyString = Parser $ \input ->
  if B.null input
  then Just (B.empty, ())
  else Nothing

charPredicate_ :: (Word8 -> Bool) -> Parser ()
charPredicate_ p = Parser $ \input -> let
  l = B.head input in
  if not (B.null input) && p l
  then Just (B.tail input, ())
  else Nothing

charPredicate :: (Word8 -> Bool) -> Parser Word8
charPredicate p = Parser $ \input -> let
  l = B.head input in
  if not (B.null input) && p l
  then Just (B.tail input, l)
  else Nothing

wordPredicate :: (Word8 -> Bool) -> Parser B.ByteString
wordPredicate p = Parser $ \input -> let
  (parsed, rest) = B.span p input
  in Just (rest, parsed)

wordPredicate1 :: (Word8 -> Bool) -> Parser B.ByteString
wordPredicate1 p = do
  parsed <- wordPredicate p
  if B.null parsed
    then empty
    else return parsed

word_ :: B.ByteString -> Parser ()
word_ w = Parser $ \input -> do
  rest <- B.stripPrefix w input
  return (rest, ())

whitespace_ :: Parser ()
whitespace_ = do
  _ <- wordPredicate I.isSpaceWord8
  return ()

char_ :: Char -> Parser ()
char_ c = charPredicate_ (== I.c2w c)

whitespace1_ :: Parser ()
whitespace1_ = do
  _ <- wordPredicate1 I.isSpaceWord8
  return ()

int :: Integral a => Parser a
int = bs2int <$> wordPredicate1 (\x -> x >= I.c2w '0' && x <= I.c2w '9')

bs2int :: Integral a => B.ByteString -> a
bs2int w = B.foldl (\start l -> start * 10 + fromIntegral (l - I.c2w '0')) 0 w

bs2afterDots :: Fractional a => B.ByteString -> a
bs2afterDots w = snd $ B.foldl f (10.0, 0) w
  where
    f (divisor, start) l =
      ( divisor * 10.0
      , start + fromIntegral (l - I.c2w '0') / divisor)

float :: Fractional a => Parser a
float = do
  before <- fromIntegral <$> int
  _ <- char_ '.'
  after <- bs2afterDots <$> wordPredicate1 (\x -> x >= I.c2w '0' && x <= I.c2w '9')
  return (before + after)

escapeString :: Parser B.ByteString
escapeString =  do
  _     <- char_ '\"'
  first <- wordPredicate $ \x -> not (x == I.c2w '\\' || x == I.c2w '\"')
  list  <- concat <$> many postslash
  _     <- char_ '\"'
  return $ B.concat (first : list)
  where
    p x = x == I.c2w 'b' ||
          x == I.c2w 'f' ||
          x == I.c2w 'n' ||
          x == I.c2w 'r' ||
          x == I.c2w 't' ||
          x == I.c2w '"' ||
          x == I.c2w '\\'

    mc x | x == I.c2w 'b' = "\b"
         | x == I.c2w 'f' = "\f"
         | x == I.c2w 'n' = "\n"
         | x == I.c2w 'r' = "\r"
         | x == I.c2w 't' = "\t"
         | x == I.c2w '"' = "\""
         | x == I.c2w '\\'= "\\"

    postslash = do
      _    <- char_ '\\'
      esc  <- mc <$> charPredicate p
      rest <- wordPredicate $ \x -> not (x == I.c2w '\\' || x == I.c2w '\"')
      return [esc, rest]
