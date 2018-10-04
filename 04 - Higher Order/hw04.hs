-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- Creates a binary tree from a list of values
foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- Inserts a Node into a binary tree while keeping it balanced
insertTree :: a -> Tree a -> Tree a
insertTree val Leaf = Node 0 Leaf val Leaf
insertTree val (Node n l x r)
    | heightL < heightR = Node n     insertL val r
    | heightL > heightR = Node n     l val insertR
    | otherwise         = Node (h+1) l val insertR
    where heightL = heightTree l 
          heightR = heightTree r
          insertL = insertTree x l
          insertR = insertTree x r
          h       = heightTree insertR 

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n l val r) = n

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- Exercise 4
-- Filters out non primes using the Sieve of Sundaram
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [ 2 * i + 1 | i <- [1..n], not $ elem i (nonPrime n)]

-- Generates a list of all non-prime numbers up to n
nonPrime :: Integer -> [Integer]
nonPrime n = [ i + j + 2 * i * j | i <- [1..n], j <- [1..n]]