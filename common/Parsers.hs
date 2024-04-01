module Parsers where

import           Control.Applicative
import qualified Data.ByteString            as B
import           Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import           Data.Word
import           ParserType

charPredicate_ :: (Char -> Bool) -> Parser ()
charPredicate_ p = Parser $ \input -> let
  l = w2c $ B.head input in
  if not (B.null input) && p l
  then Just (B.tail input, ())
  else Nothing

charPredicate :: (Char -> Bool) -> Parser Char
charPredicate p = Parser $ \input -> let
  l = w2c $ B.head input in
  if not (B.null input) && p l
  then Just (B.tail input, l)
  else Nothing

digit :: Parser Char
digit = charPredicate isDigit

integer :: Parser Integer
integer = read <$> many digit

int :: Parser Int
int = read <$> many digit

char_ :: Char -> Parser ()
char_ l = charPredicate_ (== l)

char :: Char -> Parser Char
char l = charPredicate (== l)

word :: ByteString -> Parser ByteString
word w = packChars <$> traverse char (unpackChars w)

word_ :: ByteString -> Parser ()
word_ w = foldl (<>) () <$> traverse char_ (unpackChars w)

wordS :: String -> Parser String
wordS = traverse char

wordS_ :: String -> Parser ()
wordS_ w = foldl (<>) () <$> traverse char_ w
