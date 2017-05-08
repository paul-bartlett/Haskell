-- Takes an list of Integers between 0 and 9 and outputs a vertical histogram
--histogram :: [Integer] -> String
-- do something with map +1 to an array of 0's
--histogram lst = map [maximum lst..1]
--Counts the number of occurences in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

--Creates array of number of occurences in an Int list
--ex: [1,3,4,1] -> [2,0,1,1]
countMult :: [Int] -> [Int]
countMult lst = map (`count` lst) [1..maximum lst]