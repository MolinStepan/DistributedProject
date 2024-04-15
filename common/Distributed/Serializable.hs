{-# LANGUAGE OverloadedStrings #-}

module Distributed.Serializable where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Internal   as I
import qualified Data.ByteString.Lazy.Char8 as C
import           Parsers
import           Serializers

class Serializable a where
  serializer :: Serializer a
  parser     :: Parser a
  serialize  :: a -> B.ByteString

  serialize x = B.concat $ reverse (runSerializer serializer x)
  serializer = Serializer $ \input -> [serialize input]

