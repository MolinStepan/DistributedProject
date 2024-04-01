module ParserType where

import           Control.Applicative
import           Data.ByteString

newtype Parser a = Parser { runParser :: ByteString -> Maybe (ByteString, a) }

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
