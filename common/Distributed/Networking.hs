{-# LANGUAGE OverloadedStrings #-}

module Distributed.Networking
  ( openMasterSocket
  , openSlaveSocket
  , SocketPassive
  , SocketM
  , send
  , recv
  , close
  , gracefulClose
  , Request (..)
  , RequestResult (..)
  ) where

import           Control.Concurrent
import qualified Data.ByteString           as B
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as BS

data SocketM = SocketM
  { socket :: !S.Socket
  , output :: !(Chan B.ByteString)
  , outputManager :: ThreadId
  }

newtype SocketPassive = SocketPassive
  { sock :: S.Socket }

data Request = Request
  { requestId   :: Integer
  , requestHeader :: B.ByteString
  , requestBody :: B.ByteString
  }

data RequestResult = RequestResult
  { request :: Request
  , result  :: B.ByteString
  } 

manageOutputs :: Chan B.ByteString -> S.Socket -> IO ()
manageOutputs chan sock = do
  message <- readChan chan
  if message /= "CLOSE"
    then do
      BS.send sock message
      threadDelay 100_000
      manageOutputs chan sock
    else
      S.gracefulClose sock 30_000_000

openMasterSocket :: String -> IO SocketPassive
openMasterSocket port = do
  address <- head <$> S.getAddrInfo
    (Just $ S.defaultHints {
        S.addrFlags = [S.AI_PASSIVE]   -- Passive
      , S.addrSocketType = S.Stream }) -- TCP/IP
    Nothing
    (Just port)
  sock <- S.openSocket address          -- Errors possible
  S.setSocketOption sock S.ReuseAddr 1
  S.withFdSocket sock S.setCloseOnExecIfNeeded
  S.bind sock $ S.addrAddress address
  S.listen sock 1024
  return $ SocketPassive sock

-- -- TODO rewrite
-- accepter :: SocketPassive -> Chan Slave -> IO ()
-- accepter sock slaveChan = forever $ E.bracketOnError (accept sock) (close . fst)
--         $ \(conn, _peer) -> void $ ︎
--             forkFinally (serverAction conn) (const $ gracefulClose conn 5000)

openSlaveSocket :: String -> String -> IO SocketM
openSlaveSocket ip port = do
  address <- head <$> S.getAddrInfo
    (Just $ S.defaultHints {
      S.addrSocketType = S.Stream })   -- TCP/IP
    (Just ip)
    (Just port)
  sock <- S.openSocket address          -- Errors possible
  S.connect sock $ S.addrAddress address
  channel <- newChan
  outMan  <- forkIO $ manageOutputs channel sock
  return $ SocketM sock channel outMan

close :: SocketM -> IO ()
close (SocketM sock _ worker) = do
  killThread worker
  S.close sock

gracefulClose :: SocketM -> IO ()
gracefulClose (SocketM _ chan _) = writeChan chan "CLOSE"


recv :: SocketM -> Int -> IO B.ByteString
recv (SocketM sock _ _) size = BS.recv sock size

send :: SocketM -> B.ByteString -> IO ()
send (SocketM _ chan _) msg =
  writeChan chan msg
