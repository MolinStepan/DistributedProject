module Serializers where

import qualified Data.ByteString as B

newtype Serializer a = Serializer { runSerializer :: a -> [B.ByteString]  }


