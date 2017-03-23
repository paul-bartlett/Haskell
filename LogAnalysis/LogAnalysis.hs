{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Parses individual message into a log message
parseMessage :: String -> LogMessage
parseMessage s = case words s of
    ("I":n:msg)     -> LogMessage Info (read n) (unwords $ msg)
    ("W":n:msg)     -> LogMessage Warning (read n) (unwords $ msg)
    ("E":err:n:msg) -> LogMessage (Error $ read err) (read n) (unwords $ msg)
    _               -> Unknown $ s

-- Parses several messages into an array of log messages
parse :: String -> [LogMessage]
parse s  = map parseMessage $ lines s

-- Inserts a new LogMessage into an existing sorted MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log@(LogMessage _ _ _) Leaf = Node Leaf log Leaf
insert log1@(LogMessage _ time1 _) (Node l log2@(LogMessage _ time2 _) r)
    | time1 > time2 = Node l log2 (insert log1 r)
    | otherwise     = Node (insert log1 l) log2 r
insert _ tree = tree

-- Builds a MessageTree from a list of log messages starting with a leaf
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (logmessage:logmessages) = insert logmessage (build logmessages)

-- Takes a sorted MessageTree and produces a sorted list of LogMessages from smallest to largest timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l log r) = inOrder l ++ [log] ++ inOrder r

-- Takes an unsorted list of LogMessages and returns a sorted list of messages with severity >50
whatWentWrong :: [LogMessage] -> [String]