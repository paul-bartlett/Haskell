{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

stringSplit :: String -> [String]
stringSplit s = words s

-- Match message type from letter
messageType :: String -> MessageType
messageType x = case x of
                'i' -> Info
                'w' -> Warning
                 _  -> Error

-- Parse message
parseMessage :: String -> LogMessage
parseMessage = LogMessage . take 2 . lines