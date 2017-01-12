module Main where

main :: IO ()
main = do
  putStrLn "hello world"

-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- Reverse a list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- Doubles every other number
doubleOtherNum :: [Integer] -> [Integer]
doubleOtherNum []       = []
doubleOtherNum (x:[])   = [x]
doubleOtherNum (x:y:zs) = x : (y * 2) : doubleOtherNum zs

-- Reverses a list to double each other number starting from the end, then reverses it back to the original order
doubleEveryOther :: Integer -> [Integer]
doubleEveryOther n = reverse $ doubleOtherNum $ toDigitsRev n

-- Sums a list of digits
sumNums :: [Integer] -> Integer
sumNums []     = 0
sumNums (x:xs) = x + (sumNums xs)

-- Sums a list of doubled digits
sumDigits :: Integer -> Integer
sumDigits n = sumNums $ doubleEveryOther n

-- Indicates if an integer is a valid credit card number
validate :: Integer -> Bool
validate n
    | (sumDigits n) `mod` 10 == 0 = True
    | otherwise                   = False