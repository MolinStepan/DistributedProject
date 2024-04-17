module Writer where

import Control.Concurrent
import Distributed.Networking
import Distributed.Master

successWriter :: Chan RequestResult -> IO ()
successWriter chan = do
  res <- result <$> readChan chan
  print res
  successWriter chan
  
errorLogger :: Chan RequestResult -> IO ()
errorLogger chan = do
  log <- result <$> readChan chan
  print log
  errorLogger chan

disconnectLogger :: Chan Slave -> IO ()
disconnectLogger chan = do
  log <- readChan chan
  print log
  disconnectLogger chan
