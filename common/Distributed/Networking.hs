{-# LANGUAGE OverloadedStrings #-}

module Distributed.Networking
  ( openMasterSocket
  , openSlaveSocket
  , SocketAccepter (..)
  , closeAccepter
  , accept
  , SocketM
  , send
  , recv
  , close
  , gracefulClose
  , Request (..)
  , SInt (..)
  , RequestResult (..)
  ) where

import           Control.Concurrent
import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as I
import           Distributed.Serializable
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as BS
import           Parsers

data SocketM
  = SocketM
      { socket        :: !S.Socket
      , output        :: !(Chan B.ByteString)
      , outputManager :: ThreadId
      }

newtype SocketAccepter
  = SocketAccepter S.Socket

newtype SInt
  = SInt Int
  deriving (Eq, Ord, Show)

instance Serializable SInt where
  parser = SInt <$> fixedSizeP 16 int
  serialize (SInt x) = let
    str = show x
    n = 16 - length str
    start = B.pack $ replicate n (I.c2w '0')
    in B.concat [start, B.pack $ map I.c2w str]

data Request
  = Request
      { requestId :: SInt
      , taskName  :: B.ByteString
      , taskBody  :: B.ByteString
      }

data RequestResult
  = RequestResult
      { request :: SInt
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

closeAccepter :: SocketAccepter -> Int -> IO ()
closeAccepter (SocketAccepter sock) = S.gracefulClose sock

openMasterSocket :: String -> IO SocketAccepter
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
  return $ SocketAccepter sock

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

accept :: SocketAccepter -> IO (SocketM, S.SockAddr)
accept (SocketAccepter sock) = do
  (sock', addr) <- S.accept sock
  chan <- newChan
  threadId <- forkIO $ manageOutputs chan sock'
  return ((SocketM sock' chan threadId), addr)
