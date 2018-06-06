{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid

import Buffer
import Editor
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Improved buffer to work with JoinList Size and Scrabble™ Score indexes
instance Buffer (JoinList (Score, Size) String) where
    -- Convert a JoinList buffer to a String.
    toString     = unlines . jlToList

    -- Create a JoinList buffer from a String.
    fromString   = foldr (\s acc -> acc +++ jlScoreSize s) Empty . lines
                   where jlScoreSize s = Single (scoreString s, 1) s

    -- Extract the nth line (0-indexed) from a JoinList buffer. Return Nothing
    -- for out-of-bounds indices.
    line i jl    = indexJ i jl

    -- @replaceLine n l jl returns a modified version of JoinList,
    -- with the @n@th line replaced by @l@.  If the index is
    -- out-of-bounds, the buffer should be returned unmodified.
    replaceLine n l jl = takeJ n jl +++ fromString l +++ dropJ (n+1) jl

    -- Compute the number of lines in the buffer.
    numLines     = getSize . snd . tag

    -- Compute the value of the buffer, i.e. the amount someone would
    -- be paid for publishing the contents of the buffer.
    value        = getScore . fst . tag

-- Starts the editor using JoinList for improved performance
main :: IO ()
main = runEditor editor (fromString "JoinList buffer" :: (JoinList (Score, Size) String))

-- An append function using the monoidal annotation of both lists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2

-- Gets the annotation from the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
tag _              = mempty

-- Finds the JoinList element at the specified index i.
-- (indexJ i jl) == (jlToList jl !!? i), but O(log n) versus O(n)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Single _ a) = Just a
indexJ i (Append m l1 l2)
    | i < 0 || i > sizej = Nothing
    | i < sizel1         = indexJ i l1
    | otherwise          = indexJ (i-sizel1) l2
      where sizej  = getSize $ size m
            sizel1 = getSize $ size $ tag l1
indexJ _ _ = Nothing

-- Drops the first n elements from a JoinList.
-- jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l@(Single _ _)
    | n <= 0 = l
dropJ n l@(Append m l1 l2)
    | n >= sizej  = Empty
    | n >= sizel1 = dropJ (n-sizel1) l2
    | n > 0       = dropJ n l1 +++ l2
    | otherwise   = l
      where sizej  = getSize $ size m
            sizel1 = getSize $ size $ tag l1
dropJ _ _ = Empty

-- Returns the first n elements of a JoinList.
-- jlToList (takeJ n jl) == take n (jlToList jl
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l@(Single _ _)
    | n > 0 = l
takeJ n l@(Append m l1 l2)
    | n >= sizej  = l
    | n >= sizel1 = l1 +++ takeJ (n-sizel1) l2
    | n > 0       = takeJ n l1
      where sizej  = getSize $ size m
            sizel1 = getSize $ size $ tag l1
takeJ _ _ = Empty

-- Uses Scrabble™ rules to use word scores as indexes
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Safe indexing function used for testing other functions
-- where indexJ i jl           == jlToList jl !!? i,
--       jlToList (dropJ n jl) == drop n (jlToList jl),
--       jlToList (takeJ n jl) == take n (jlToList jl
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

-- Used to test indexJ, takeJ, and dropJ
test1 :: JoinList Size String
test1 = Append (Size 4)
            (Append (Size 3)
                (Single (Size 1) "y")
                (Append (Size 2) 
                    (Single (Size 1) "e")
                    (Single (Size 1) "a")
                )
            )
            (Single (Size 1) "h")
