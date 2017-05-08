{-# OPTIONS_GHC -Wall #-}
--Counts the number of occurences in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

--Creates array of number of occurences in an Int list
--ex: [1,3,4,1,9] -> [0,2,0,1,1,0,0,0,0,1]
countMult :: [Int] -> [Int]
countMult lst = map (`count` lst) [1..9]

--Creates the string for outputting
outputSymbol :: Integer -> [Integer] -> String
outputSymbol num (x:xs)
    | x < num    = " " ++ outputSymbol num xs
    | x >= num   = "*" ++ outputSymbol num xs
outputSymbol _ _ = []

--Takes an list of Integers between 0 and 9 and outputs a vertical histogram
histogram :: [Integer] -> String
histogram lst = foldr (++) [] $ map (`outputSymbol` lst) [1..9]