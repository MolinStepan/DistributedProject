{-# LANGUAGE OverloadedStrings #-}

module Distributed.Slave where

import           Distributed.Networking
import           Network.Socket

a :: Request
a = Request (SInt 10) "123" "1234"


-- startServer :: HostName -> IO ()
