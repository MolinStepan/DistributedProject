{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Network.Socket
import           Network.Socket.ByteString
import           System.Environment


main :: IO ()
main = do
  [masterIp, masterPort, masterPassword] <- getArgs

  main
-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      connect sock $ addrAddress addr
      return sock
