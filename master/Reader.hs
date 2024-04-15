module Reader where

import Control.Concurrent
import System.IO
import Distributed.Master
import qualified Data.ByteString.Char8 as C

reader :: Chan Request -> IO ()
reader output = do
  contents <- (map C.pack) . (words) <$> readFile "test.txt"
  let nums = fromIntegral <$> [1..length contents]
  let res = zipWith (\num cont -> Request num (C.pack "Prime") cont) nums contents
  writeList2Chan output res
  

  
