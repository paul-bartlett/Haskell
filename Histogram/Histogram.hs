{-# OPTIONS_GHC -Wall #-}
--Counts the number of occurences in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

--Creates array of number of occurences in an Integer list
--ex: [1,3,4,1,9] -> [0,2,0,1,1,0,0,0,0,1]
countMult :: [Integer] -> [Integer]
countMult lst = map (toInteger) $ map (`count` lst) [0..9]

--Creates the string for outputting
outputSymbol :: Integer -> [Integer] -> String
outputSymbol num (x:xs)
    | x < num    = " " ++ outputSymbol num xs
    | x >= num   = "*" ++ outputSymbol num xs
outputSymbol _ _ = "\n"

histoPlot :: [Integer] -> String
histoPlot lst = foldr (++) [] $ map (`outputSymbol` lst) [maximum lst, maximum lst - 1..1]

--Takes an list of Integers between 0 and 9 and outputs a vertical histogram
histogram :: [Integer] -> String
histogram lst = (histoPlot $ countMult lst) ++ "==========\n0123456789\n"