{-# LANGUAGE OverloadedStrings #-}

module Distributed.Accepter where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad
import qualified Data.Set                  as Set
import           Distributed.Networking
import           Parsers
import qualified Data.ByteString           as B
import           Distributed.Master
import Distributed.Serializable

-- accepter = undefined

accepter :: SocketPassive
         -> Int
         -> Chan Request
         -> Chan RequestResult
         -> Chan RequestResult
         -> Chan Slave
         -> IO ()
accepter sock port pool success error disconnected = forever $
  E.bracketOnError
    (accept sock)
    (closep . fst)
    (\(conn, _) -> forkFinally (serverAction conn) (const $ gclosep conn port))
  where
    serverAction conn = handleSlave conn port (Slave 1) pool disconnected



handleSlave :: SocketPassive -> Int -> Slave -> Chan Request -> Chan Slave -> IO ()
handleSlave sock port id requestPool disconnectedServers = do
  alive <- newMVar ()
  currentlyProcessing <- newMVar Set.empty
  terminate <- newEmptyMVar :: IO (MVar ())
  stopping <- newEmptyMVar :: IO (MVar ())
  socketMutex <- newEmptyMVar :: IO (MVar())

  t1 <- forkIO $ ping alive terminate socketMutex
  t2 <- forkIO $ sendRequests currentlyProcessing requestPool stopping
  t3 <- forkIO $ acceptSlaveInfo currentlyProcessing alive stopping terminate
  _ <- takeMVar terminate
  writeChan disconnectedServers id
  gclosep sock port

  where
    ping alive terminate socketMutex = do
      putMVar socketMutex ()
      sendToSlave sock "Status"
      _ <- takeMVar socketMutex
      _ <- takeMVar alive
      threadDelay 300_000_000
      stop <- isEmptyMVar alive
      if stop
        then putMVar terminate ()
        else ping alive terminate socketMutex

    sendRequests :: MVar (Set.Set TaskId) -> Chan Request -> MVar () -> IO ()
    sendRequests currentlyProcessing requestPool stopping = do
      set <- takeMVar currentlyProcessing
      if Set.size set < 5
        then do
        putMVar currentlyProcessing set
        task <- readChan requestPool 
        sendToSlave sock (serialize task)
        set <- takeMVar currentlyProcessing
        putMVar currentlyProcessing (Set.insert (requestId task) set)
        else
        putMVar currentlyProcessing set
      threadDelay 1_000_000

    acceptSlaveInfo currentlyProcessing alive stopping terminate = do
      inp <- hear sock 64
      if inp == "OK"
        then putMVar alive ()
        else undefined -- there will be completed task or error message

