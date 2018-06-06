{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

score :: Char -> Score
score c
    | lc `elem` "aeilnorstu" = Score 1
    | lc `elem` "dg"         = Score 2
    | lc `elem` "bcmp"       = Score 3
    | lc `elem` "fhvwy"      = Score 4
    | lc `elem` "k"          = Score 5
    | lc `elem` "jx"         = Score 8
    | lc `elem` "qz"         = Score 10
    | otherwise              = Score 0
      where lc = toLower c

scoreString :: String -> Score
scoreString = foldr (+) (Score 0) score