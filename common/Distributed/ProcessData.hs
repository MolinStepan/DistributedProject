{-# LANGUAGE OverloadedStrings #-}

module Distributed.ProcessData where

import           Control.Applicative
import           Control.Concurrent
import qualified Data.ByteString      as B
import           Distributed.Networking
import           Parsers
import           Distributed.Serializable
-- import           Tasks

-- processData :: Process -> Chan B.ByteString -> B.ByteString -> IO ThreadId
-- processData process outputChan input = forkIO $ 
--   let res = process input
--   in case res of
--     Just str -> writeChan outputChan str
--     Nothing  -> writeChan outputChan "Error"

type Process = B.ByteString -> Maybe B.ByteString


