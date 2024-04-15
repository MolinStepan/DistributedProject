{-# LANGUAGE OverloadedStrings #-}

module Distributed.Master where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.Set                  as Set
import           Parsers
import qualified Data.ByteString           as B
import qualified Control.Exception         as E
import           Distributed.Networking

data Request = Request
  { requestId   :: Integer
  , requestHeader :: B.ByteString
  , requestBody :: B.ByteString
  }

data RequestResult = RequestResult
  { request :: Request
  , result  :: B.ByteString
  } 

newtype Slave = Slave { getInfo :: Int } deriving (Show, Eq)

accepter :: SocketM -> Chan Request -> Chan RequestResult -> IO ()
accepter = undefined

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




















{-
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port serverAction = do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (serverAction conn) (const $ gracefulClose conn 5000)
-- let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream -- }
--   addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
--   Network.Socket.bind sock (addrAddress addr)
--   getSocketName sock
-}
