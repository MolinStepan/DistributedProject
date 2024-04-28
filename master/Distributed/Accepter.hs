{-# LANGUAGE OverloadedStrings #-}

module Distributed.Accepter where

import           Control.Concurrent
import qualified Control.Exception        as E
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.Set                 as Set
import           Distributed.Master
import           Distributed.Networking
import           Distributed.Serializable
import           Parsers

-- accepter = undefined

runAccepter :: SocketAccepter
         -> Int
         -> Chan Request
         -> Chan RequestResult
         -> Chan RequestResult
         -> Chan Slave
         -> IO ()
runAccepter sock port pool success error disconnected = forever $
  E.bracketOnError
    (accept sock)
    (close . fst)
    (\(conn, _) -> forkFinally (serverAction conn) (const $ gracefulClose conn {-port-}))
  where
    serverAction conn = handleSlave conn port (Slave 1) pool disconnected



handleSlave :: SocketM -> Int -> Slave -> Chan Request -> Chan Slave -> IO ()
handleSlave sock port id requestPool disconnectedServers = do
  alive <- newMVar ()
  currentlyProcessing <- newMVar Set.empty
  terminate <- newEmptyMVar :: IO (MVar ())
  stopping <- newEmptyMVar :: IO (MVar ())

  t1 <- forkIO $ ping alive terminate
  t2 <- forkIO $ sendRequests currentlyProcessing requestPool stopping
  t3 <- forkIO $ acceptSlaveInfo currentlyProcessing alive stopping terminate
  _ <- takeMVar terminate

  writeChan disconnectedServers id
  gracefulClose sock {-port-}

  where
    ping :: MVar () -> MVar () -> IO ()
    ping alive terminate = do
      send sock "Status"
      _ <- takeMVar alive
      threadDelay 300_000_000
      stop <- isEmptyMVar alive
      if stop
        then putMVar terminate ()
        else ping alive terminate

    sendRequests :: MVar (Set.Set SInt) -> Chan Request -> MVar () -> IO ()
    sendRequests currentlyProcessing requestPool stopping = do
      set <- takeMVar currentlyProcessing
      if Set.size set < 5
        then do
        putMVar currentlyProcessing set
        Request id task body <- readChan requestPool
        send sock $ B.concat [serialize id, task, serialize . SInt $ B.length body]
        send sock $ B.concat [serialize id, body]
        set <- takeMVar currentlyProcessing
        putMVar currentlyProcessing (Set.insert id set)
        else
        putMVar currentlyProcessing set
      threadDelay 1_000_000

    acceptSlaveInfo :: MVar (Set.Set SInt) -> MVar () -> MVar () -> MVar () -> IO ()
    acceptSlaveInfo currentlyProcessing alive stopping terminate = do
      inp <- recv sock 64
      if inp == "OK"
        then putMVar alive ()
        else undefined -- there will be completed task or error message

