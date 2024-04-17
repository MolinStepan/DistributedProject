{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Exception         as E
import           Control.Monad
import           Distributed.Master
import Distributed.Networking
import Reader
import Writer
import System.Environment
import Distributed.Accepter

-- TODO
-- map of active connections
-- task distribution (channel?)
-- task id
-- database result storage



main :: IO ()
main = do
  [port] <- getArgs

  successTasks <- newChan :: IO (Chan RequestResult)
  failedTasks  <- newChan :: IO (Chan RequestResult)
  requestsPool <- newChan :: IO (Chan Request)
  stop         <- newEmptyMVar :: IO (MVar ())
  disconnectedServers <- newChan :: IO (Chan Slave)

  sock <- openMasterSocket port

  forkIO $ reader requestsPool
  forkIO $ accepter sock (read port) requestsPool successTasks failedTasks disconnectedServers
  forkIO $ successWriter successTasks
  forkIO $ errorLogger failedTasks
  forkIO $ disconnectLogger disconnectedServers 

  _ <- takeMVar stop
  print "Closed"



