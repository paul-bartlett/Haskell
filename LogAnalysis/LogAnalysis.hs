{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Parse message
parseMessage :: String -> LogMessage
parseMessage = LogMessage . take 2 . lines