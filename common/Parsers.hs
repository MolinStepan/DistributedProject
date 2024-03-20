module Parsers where

import           Data.ByteString

newtype Parser a = Parser { runParser :: ByteString -> Maybe (ByteString, a) }

instance Functor Parser where
  fmap f (Parser g) = Parser $ (fmap . fmap . fmap $ f) g

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  f <*> g = Parser $ \input -> do
    (rest, parsedFunc) <- runParser f input
    (rest, parsedVal)  <- runParser g rest
    return (rest, parsedFunc parsedVal)
