module Distributed.Networking (
    openMasterSocket
  , openSlaveSocket
  , Socket
  , send
  , recv
  ) where

import           Control.Concurrent
import qualified Data.ByteString           as BS
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as B

data Socket = Socket {
    socket       :: S.Socket
  , sendMutex    :: MVar ()
}

openMasterSocket :: String -> IO Socket
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
  Socket sock <$> newEmptyMVar

openSlaveSocket :: String -> String -> IO Socket
openSlaveSocket ip port = do
  address <- head <$> S.getAddrInfo
    (Just $ S.defaultHints {
        S.addrFlags = [S.AI_PASSIVE]   -- Passive
      , S.addrSocketType = S.Stream }) -- TCP/IP
    (Just ip)
    (Just port)
  sock <- S.openSocket address          -- Errors possible
  S.connect sock $ S.addrAddress address
  Socket sock <$> newEmptyMVar

recv :: Socket -> Int -> IO BS.ByteString
recv (Socket sock _) size = B.recv sock size

send :: Socket -> BS.ByteString -> IO ()
send = undefined
