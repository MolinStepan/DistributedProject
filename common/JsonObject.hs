{-# LANGUAGE OverloadedStrings #-}

module JsonObject (
    JsonObject
  , jsonObject
) where

import           Control.Applicative
import qualified Data.ByteString     as B
import           Parsers

data JsonObject
  = JsonCustomObject [(B.ByteString, JsonObject)]
  | JsonInt Int
  | JsonFloat Float
  | JsonString B.ByteString
  | JsonBool Bool
  | JsonNull
  | JsonArray [JsonObject]
  deriving (Show, Eq)

jsonObject :: Parser JsonObject
jsonObject = jsonFloat <|> jsonInt <|> jsonString <|> jsonArray <|> jsonCustomObject

jsonInt :: Parser JsonObject
jsonInt = JsonInt <$> int

jsonNull :: Parser JsonObject
jsonNull = JsonNull <$ word_ "null"

jsonBool :: Parser JsonObject
jsonBool = (JsonBool True <$ word_ "true") <|> (JsonBool False <$ word_ "false")

-- FIXME no support for exponential notation
jsonFloat :: Parser JsonObject
jsonFloat = JsonFloat <$> float

jsonString :: Parser JsonObject
jsonString = JsonString <$> escapeString

jsonArray :: Parser JsonObject
jsonArray = jsonEmptyArray <|> jsonNonEmptyArray

jsonEmptyArray :: Parser JsonObject
jsonEmptyArray = JsonArray <$> do
  _ <- char_ '['
  _ <- whitespace_
  _ <- char_ ']'
  return []

jsonNonEmptyArray :: Parser JsonObject
jsonNonEmptyArray = JsonArray <$> do
  _     <- char_ '[' *> whitespace_
  first <- jsonObject
  rest  <- many $ whitespace_ *> char_ ',' *> whitespace_ *> jsonObject
  _     <- whitespace_ *> char_ ']'
  return (first : rest)

jsonCustomObject :: Parser JsonObject
jsonCustomObject = jsonNonEmptyObject <|> jsonEmptyObject

jsonEmptyObject :: Parser JsonObject
jsonEmptyObject = JsonCustomObject <$> do
  _ <- char_ '{' *> whitespace_ *> char_ '}'
  return []

jsonNonEmptyObject :: Parser JsonObject
jsonNonEmptyObject = JsonCustomObject <$> do
  _ <- char_ '{' *> whitespace_
  first <- parsePair
  rest <- many $ whitespace_ *> char_ ',' *> whitespace_ *> parsePair
  _ <- whitespace_ *> char_ '}'
  return (first : rest)
  where
    parsePair = (,) <$> escapeString <* whitespace_ <* char_ ':' <* whitespace_ <*> jsonObject
