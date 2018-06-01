{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser

-- Exercise 1
-- Adds and multiplies basic expressions of type ExprT
eval :: ExprT -> Integer
eval (Lit val)       = val
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)

-- Exercise 2
-- Parses expression and evaluates if valid
evalStr :: String -> Maybe Integer
evalStr s = case p of
          Just val -> Just (eval val)
          Nothing  -> Nothing
          where p = parseExp Lit Add Mul s