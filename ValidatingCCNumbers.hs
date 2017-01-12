module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0 	= []
	| otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev	:: Integer -> [Integer]
