{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe

-- Exercise 1
-- Adds and multiplies basic expressions of type ExprT
eval :: ExprT -> Integer
eval (ExprT.Lit val)       = val
eval (ExprT.Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (ExprT.Mul exp1 exp2) = (eval exp1) * (eval exp2)

-- Exercise 2
-- Parses expression and evaluates if valid
evalStr :: String -> Maybe Integer
evalStr s = case p of
          Just val -> Just (eval val)
          Nothing  -> Nothing
          where p = parseExp ExprT.Lit ExprT.Add ExprT.Mul s

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where 
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
    lit val = val
    add exp1 exp2 = exp1 + exp2
    mul exp1 exp2 = exp1 * exp2

instance Expr Bool where
    lit val
        | val <= 0  = False
        | otherwise = True
    add exp1 exp2 = exp1 || exp2
    mul exp1 exp2 = exp1 && exp2

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax exp1) (MinMax exp2) = MinMax (max exp1 exp2)
    mul (MinMax exp1) (MinMax exp2) = MinMax (min exp1 exp2)

instance Expr Mod7 where
    lit val = Mod7 (val `mod` 7)
    add (Mod7 exp1) (Mod7 exp2) = Mod7 ((exp1 + exp2) `mod` 7)
    mul (Mod7 exp1) (Mod7 exp2) = Mod7 ((exp1 * exp2) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

-- Exercise 5
instance Expr Program where
    lit val = [StackVM.PushI val]
    add exp1 exp2 = exp1 ++ exp2 ++ [StackVM.Add]
    mul exp1 exp2 = exp1 ++ exp2 ++ [StackVM.Mul]

testProg :: Maybe StackVM.Program
testProg = testExp

compileProg :: String -> Either String StackVal
compileProg = stackVM . fromJust . compile

compile :: String -> Maybe Program
compile = parseExp lit add mul