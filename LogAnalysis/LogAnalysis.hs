{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Parse int for Error ID and the stamp number
intParse :: [String] -> Int
intParse s = read $ head s

-- Match message type from letter and parses into a log message
messageType :: String -> [String] -> LogMessage
messageType x s = case x of
                "I" -> LogMessage Info (intParse s) (unwords $ tail s)
                "W" -> LogMessage Warning (intParse s) (unwords $ tail s)
                "E" -> LogMessage (Error (intParse s)) (intParse $ tail s) (unwords $ drop 2 s)
                _   -> Unknown $ unwords (x:s)

-- Sends message type and remainder of message to be parsed
parseMessage :: String -> LogMessage
parseMessage s = messageType (head $ words s) (tail $ words s)

parse :: String -> [LogMessage]
parse s  = [parseMessage s]