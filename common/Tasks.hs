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

data Task = Task {
    inputParser  :: Parser Input
  , inputCoder :: Input -> B.ByteString
  , compute      :: Input -> Answer
  , answerParser :: Parser Answer
  , answerCoder :: Answer -> B.ByteString
  }

allTasks :: [(B.ByteString, Task)]
allTasks = []

