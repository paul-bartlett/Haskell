{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Match message type from letter
messageType :: String -> [String] -> LogMessage
messageType x s = case x of
                "I" -> LogMessage Info (stampParse s) (unwords $ tail s)
                "W" -> LogMessage Warning (stampParse s) (unwords $ tail s)
                --'E' -> LogMessage Error (read head s) stampParse $ tail s

-- Parse the stamp number
stampParse :: [String] -> TimeStamp
stampParse s = read $ head s

-- Parse message
parseMessage :: String -> LogMessage
parseMessage s = messageType (head $ words s) (tail $ words s)