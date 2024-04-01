{-# LANGUAGE OverloadedStrings #-}

module Distributed.ProcessData where

import           Control.Applicative
import qualified Data.ByteString     as B
import           Network.Socket
import           Parsers
import           ParserType
import           Tasks

task :: (B.ByteString, Task) -> Parser Task
task (s, t) = Parser $ \input -> do
  (rest, _) <- runParser (word_ s) input
  return (rest, t)

getTask :: Parser Task
getTask = foldl (<|>) empty $ map task allTasks
