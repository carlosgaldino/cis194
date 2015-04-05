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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert nlog Leaf = Node Leaf nlog Leaf
insert nlog (Node left log right)
  | getTimeStamp nlog > getTimeStamp log  = Node left log (insert nlog right)
  | getTimeStamp nlog <= getTimeStamp log = Node (insert nlog left) log right

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left l right) = inOrder left ++ [l] ++ inOrder right

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _                          = False

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error i) _ _)
  | i >= 50   = True
  | otherwise = False
isSevere _    = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = map getMessage errors
  where errors = inOrder . build $ filter (\x -> isSevere x && isError x) ls
