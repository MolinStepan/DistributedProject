{-# LANGUAGE OverloadedStrings #-}

module Distributed.ProcessData where

import qualified Data.ByteString      as B
 
type Process = B.ByteString -> Maybe B.ByteString


