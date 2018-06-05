{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import StringBuffer
import Editor
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) list1 list2 = Append (tag list1 <> tag list2) list1 list2

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m
tag _              = mempty