{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words
  where parseMessage' ("I":time:msg)       = LogMessage Info (read time) $ unwords msg
        parseMessage' ("W":time:msg)       = LogMessage Warning (read time) $ unwords msg
        parseMessage' ("E":level:time:msg) = LogMessage (Error $ read level) (read time) $ unwords msg
        parseMessage' msg                  = Unknown $ unwords msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines
