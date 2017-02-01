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

parse :: String -> [LogMessage]
parse s  = map parseMessage $ lines s