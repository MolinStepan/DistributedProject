{-# LANGUAGE OverloadedStrings #-}

module Tasks where

import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import           Parsers
import           Distributed.Serializable  
import           Control.Applicative
import           Distributed.ProcessData


newtype Task a b = Task { compute :: a -> b }

instance Serializable Integer where
  parser = int
  serialize = C.pack . show 

instance Serializable Bool where
  parser = (True <$ word_ "True") <|> (False <$ word_ "False")
  serialize True = "True"
  serialize False = "False"

checkPrime :: Integer -> Bool
checkPrime num =
  odd num && helper primes num
  where
    helper (div : rest) num = 
      div^2 > num ||
      num `mod` div /= 0 &&
      helper rest num

primes :: [Integer]
primes = 2 : oddPrimes 3
  where
    oddPrimes n =
      if checkPrime n
      then n : oddPrimes (n+2)
      else oddPrimes (n+2)
          
toProcess :: (Serializable a, Serializable b) => Task a b -> Process
toProcess task input = do
  ("", inp) <- runParser parser input
  return $ serialize (compute task inp)

allTasks :: [(B.ByteString, Process)]
allTasks = 
  [ ("Prime", toProcess $ Task checkPrime)
  ]


