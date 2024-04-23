{-# LANGUAGE OverloadedStrings #-}

module Distributed.Networking
  ( openMasterSocket
  , openSlaveSocket
  , SocketPassive (..)
  , accept
  , gclosep
  , sendToSlave
  , hear
  , SocketM
  , send
  , recv
  , closep
  , close
  , gracefulClose
  , RequestHeader (..)
  , SInt (..)
  , RequestResult (..)
  ) where

import           Control.Concurrent
import qualified Data.ByteString           as B
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as BS
import qualified Data.ByteString.Internal  as I
import Distributed.Serializable
import Parsers

data SocketM = SocketM
  { socket :: !S.Socket
  , output :: !(Chan B.ByteString)
  , outputManager :: ThreadId
  }

newtype SocketPassive = SocketPassive
  { sock :: S.Socket }

newtype SInt = SInt Int deriving (Eq, Show, Ord)
instance Serializable SInt where
  parser = SInt <$> fixedSizeP 16 int
  serialize (SInt x) = let
    str = show x
    n = 16 - length str
    start = B.pack $ replicate n (I.c2w '0')
    in B.concat [start, B.pack $ map I.c2w str]

data RequestHeader = RequestHeader
  { requestId   :: SInt
  , taskName :: B.ByteString
  , bodySize :: SInt
  } deriving (Eq, Show)

instance Serializable RequestHeader where 
  serialize (RequestHeader id name len) = B.concat [serialize id, name, serialize len]
  parser = do
    tId <- parser
    head <- fixedSize id 16
    len <- parser
    return $ RequestHeader tId head len
  
data RequestResult = RequestResult
  { request :: RequestHeader
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

recv (SocketM sock _ _) size = BS.recv sock size

send :: SocketM -> B.ByteString -> IO ()
send (SocketM _ chan _) msg =
  writeChan chan msg

accept (SocketPassive sock) = do
  (s, a) <- S.accept sock
  return (SocketPassive s, a)


closep (SocketPassive sock) = S.close sock
gclosep (SocketPassive sock) = S.gracefulClose sock
sendToSlave (SocketPassive sock) = BS.send sock
hear (SocketPassive sock) = BS.recv sock
