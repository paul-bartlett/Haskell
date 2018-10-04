{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Party where

import Employee

import Data.Tree

-- Adds an Employee to the GuestList
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps f) = GL (emp : emps) (f + empFun emp)

-- Monoid instance for empty GuestList and appending 2 GuestLists
instance Monoid GuestList where
    mempty  = GL [] 0
    mappend (GL emps1 f1) (GL emps2 f2) = GL (emps1 ++ emps2) (f1 + f2)

-- Takes two GuestLists and return the one that is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 
    | gl1 > gl2 = gl1
    | otherwise = gl2

-- Fold function for Data.Tree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go where
    go (Node x ts) = f x (map go ts)

-- Takes an employee and the optimal GuestList for each subdivision under him
combineGLs :: Employee -> [GuestList] -> GuestList