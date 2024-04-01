{-# LANGUAGE OverloadedStrings #-}

module Tasks where

import qualified Data.ByteString as B
import           Parsers

data Input = IInteger Integer
           | IString String
           | IBString B.ByteString

data Answer = AInteger Integer
            | AString String
            | ABString B.ByteString

newtype Task = Task { compute :: Input -> Answer }

allTasks :: [(B.ByteString, Task)]
allTasks = []
