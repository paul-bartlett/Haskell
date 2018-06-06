{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid

import StringBuffer
import Editor
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

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

test1 = Append (Size 4)
            (Append (Size 3)
                (Single (Size 1) "y")
                (Append (Size 2) 
                    (Single (Size 1) "e")
                    (Single (Size 1) "a")
                )
            )
            (Single (Size 1) "h")