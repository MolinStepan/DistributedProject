{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Exception         as E
import           Control.Monad
import           Network.Socket as S
import           Network.Socket.ByteString


-- placeholders
main :: IO ()
main = runTCPServer Nothing "3000" serverAction
  where
    serverAction sock = do
      _ <- getLine
      msg <- recv sock 1024
      print msg
      serverAction sock


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
-- let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
--   addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
--   Network.Socket.bind sock (addrAddress addr)
--   getSocketName sock
