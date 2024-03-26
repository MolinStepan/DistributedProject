{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import qualified Control.Exception         as E
import           Network.Socket
import           Network.Socket.ByteString
import           System.Environment

-- placeholders
talk s str = do
  sendAll s str
  threadDelay 100_000
  talk s str

abob s1 s2 s = do
  forkIO $ talk s s1
  talk s s2

main :: IO ()
main = do
  [masterIp, masterPort, masterPassword] <- getArgs
  runTCPClient masterIp masterPort $ abob "AAAAAAAAA" "BBBBBBBBB"

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
