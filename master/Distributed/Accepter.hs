{-# LANGUAGE OverloadedStrings #-}

module Distributed.Accepter where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.Set                  as Set
import           Distributed.Networking
import           Parsers
import qualified Data.ByteString           as B
import           Distributed.Master

accepter :: SocketPassive
         -> Chan Request
         -> Chan RequestResult
         -> Chan RequestResult
         -> Chan Slave
         -> IO ()
accepter pool success error disconnected = undefined


handleSlave :: SocketM -> Slave -> Chan Request -> Chan Slave -> IO ()
handleSlave sock id requestPool disconnectedServers = do
  alive <- newMVar ()
  currentlyProcessing <- newMVar Set.empty
  terminate <- newEmptyMVar :: IO (MVar ())
  stopping <- newEmptyMVar :: IO (MVar ())
  socketMutex <- newEmptyMVar :: IO (MVar())

  forkIO $ ping alive terminate socketMutex
  forkIO $ sendRequests currentlyProcessing requestPool stopping
  forkIO $ acceptSlaveInfo currentlyProcessing alive stopping terminate
  _ <- takeMVar terminate
  writeChan disconnectedServers id
  gracefulClose sock 

  where
    ping alive terminate socketMutex = do
      putMVar socketMutex ()
      send sock "Status"
      _ <- takeMVar socketMutex
      _ <- takeMVar alive
      threadDelay 300_000_000
      stop <- isEmptyMVar alive
      if stop
        then putMVar terminate ()
        else ping alive terminate socketMutex

    sendRequests currentlyProcessing requestPool stopping = undefined

    acceptSlaveInfo currentlyProcessing alive stopping terminate = do
      inp <- recv sock 64
      if inp == "OK"
        then putMVar alive ()
        else undefined -- there will be completed task or error message

