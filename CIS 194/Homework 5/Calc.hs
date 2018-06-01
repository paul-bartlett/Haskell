{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT

-- Exercise 1
-- Adds and multiplies basic expressions of type ExprT
eval :: ExprT -> Integer
eval (Lit val) = val
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)