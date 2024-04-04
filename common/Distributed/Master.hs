{-# LANGUAGE OverloadedStrings #-}

module Distributed.Master where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.Set                  as Set
import           Network.Socket
import           Network.Socket.ByteString
import           Parsers

data Request = Request {
    requestId   :: Integer
  , requestType :: String
  , requestBody :: String
}


-- stolen placeholder
runMaster :: ServiceName -> IO ()
runMaster port = do
    requestPool <- newChan :: IO (Chan Request)
    addr <- head <$> getAddrInfo (Just hints) Nothing (Just port)
    bracket (open addr) close $ manageConnections requestPool
  where
    hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    manageConnections requestPool sock  = forever $ bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkIO (handleServer conn requestPool)



handleServer :: Socket -> Chan Request -> IO ()
handleServer sock requestPool = do
  alive <- newMVar ()
  currentlyProcessing <- newMVar Set.empty
  terminate <- newEmptyMVar :: IO (MVar ())
  stopping <- newEmptyMVar :: IO (MVar ())
  socketMutex <- newEmptyMVar :: IO (MVar())

  forkIO $ ping alive terminate socketMutex
  forkIO $ sendRequests currentlyProcessing requestPool stopping
  forkIO $ acceptSlaveInfo currentlyProcessing alive stopping terminate
  _ <- takeMVar terminate
  gracefulClose sock 5000

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
