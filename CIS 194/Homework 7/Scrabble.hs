{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

-- Gets score for character based on Scrabble™ rules
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

-- Gets Scrabble™ score for a word
scoreString :: String -> Score
scoreString = foldr (\x acc -> acc + score x) $ Score 0