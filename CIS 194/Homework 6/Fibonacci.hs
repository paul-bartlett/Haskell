{-# OPTIONS_GHC -Wall #-}

module Fibonacci where

-- Exercise 1
-- A recursive function to determine the nth Fibonacci number
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- An infinite list of all Fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
-- An efficient method of listing Fibonacci numbers
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4
-- Creates a stream from a single element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Generates a Stream from a seed
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f $ f x)