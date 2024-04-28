module Reader where

import           Control.Concurrent
import qualified Data.ByteString.Char8  as C
import           Distributed.Networking
import           System.IO

reader :: Chan Request -> IO ()
reader output = do
  contents <- (map C.pack) . (words) <$> readFile "test.txt"
  let nums = SInt . fromIntegral <$> [1..length contents]
  let res = zipWith (\num cont -> Request num (C.pack "Prime") cont) nums contents
  writeList2Chan output res



